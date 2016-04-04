object Jdbc {
  import scalikejdbc._

  // initialize JDBC driver & connection pool
  Class.forName("org.h2.Driver")
  ConnectionPool.singleton("jdbc:h2:mem:hello", "user", "pass")

  // ad-hoc session provider on the REPL
  implicit val session = AutoSession

  def createTable = {
    // table creation, you can run DDL by using #execute as same as JDBC
    sql"""
    create table members (
      id serial not null primary key,
      name varchar(64),
      created_at timestamp not null
    )
    """.execute.apply()
  }

  def insertData = {
    // insert initial data
    Seq("Alice", "Bob", "Chris") foreach { name =>
      sql"insert into members (name, created_at) values (${name}, current_timestamp)".update.apply()
    }
  }

  // for now, retrieves all data as Map value
  def entities: List[Map[String, Any]] = sql"select * from members".map(_.toMap).list.apply()

  import scalikejdbc.jsr310._
  import java.time._

  case class Member(id: Long, name: Option[String], createdAt: ZonedDateTime)

  object Member {
    def apply(rs: WrappedResultSet) = new Member(
      rs.long("id"), rs.stringOpt("name"), rs.dateTime("created_at")
    )
  }

  // Get all members
  def getMembers: List[Member] = sql"select * from members".map(rs => Member(rs)).list.apply()

  // Find a member
  def findMember(id: Long) = {
    val query = sql"""
      SELECT *
      FROM members
      WHERE id=${id}"""

    query.map(rs => Member(rs)).single.apply()
  }

  // def getMembers2 = {
  //   val m = Member.syntax("m")
  //   val members = withSQL {
  //     select
  //       .from(Member as m)
  //       .where
  //       .eq(m.id, 2)
  //   }.map(Member(m)).list.apply()
  // }

  def findMembers(ids: Seq[Int]) = {
    val query = sql"SELECT * FROM members WHERE id IN (${ids})"

    query.map(rs => Member(rs)).list.apply()
  }

  def membersByCreation = {
    val column = sqls"current_timestamp"
    val ordering = sqls"ASC"
    val query = sql"SELECT * FROM members ORDER BY ${column} ${ordering}"

    query.map(rs => Member(rs)).list.apply()
  }

  def upsertMember(m: Member) = {
    DB localTx { implicit session =>
      val rowsAffected = sql"UPDATE members SET name = ${m.name} WHERE id = ${m.id}".update.apply()

      if (rowsAffected == 0) {
        val id = sql"INSERT INTO members (name, created_at) VALUES (${m.name}, current_timestamp)"
          .updateAndReturnGeneratedKey.apply()
        id
      } else m.id
    }
  }
}

object QueryDSL {
  trait Statement {
    def toStatement: (String, Vector[Any])
  }

  case class Column(columnName: String) {
    private def withColumn(operator: String, column: Column) = ColumnStatement(
      s"$columnName $operator ${column.columnName}", None, Vector()
    )
    private def withValue(operator: String, value: Any) = ColumnStatement(
      s"$columnName $operator ?", Some(value), Vector()
    )

    def eq(column: Column): ColumnStatement = withColumn("=", column)
    def eq(value: Any): ColumnStatement = withValue("=", value)

    def gt(column: Column): ColumnStatement = withColumn(">", column)
    def gt(value: Any): ColumnStatement = withValue(">", value)

    def lt(column: Column): ColumnStatement = withColumn("<", column)
    def lt(value: Any): ColumnStatement = withValue("<", value)

    def asc: ColumnStatement = ColumnStatement(s"$columnName ASC", None, Vector())
    def desc: ColumnStatement = ColumnStatement(s"$columnName DESC", None, Vector())

    def like(value: String): ColumnStatement = withValue("LIKE", value)
  }

  case class ColumnStatement(
    sql: String,
    placeHolder: Option[Any],
    subStatements: Vector[(String, ColumnStatement)]
  ) extends Statement {

    def toStatement = {
      val combinedSql = sql + subStatements.foldLeft("") {
        case (acc, (operator, statement)) => s"$acc $operator ${statement.sql}"
      }

      val subPlaceHolders = for {
        (_, statement) <- subStatements
        placeHolder <- statement.placeHolder
      } yield placeHolder

      (combinedSql, placeHolder.toVector ++ subPlaceHolders)
    }

    def and(subStatement: ColumnStatement): ColumnStatement = this.copy(
      subStatements = subStatements :+ Tuple2("AND", subStatement)
    )

    def or(subStatement: ColumnStatement): ColumnStatement = this.copy(
      subStatements = subStatements :+ Tuple2("OR", subStatement)
    )
  }

  trait Join extends Statement

  case class LeftJoin(tableName: String, on: ColumnStatement) extends Join {
    def toStatement = {
      val (onSql, onPlaceHolders) = on.toStatement
      (s"LEFT JOIN $tableName ON $onSql", onPlaceHolders)
    }
  }

  case class RightJoin(tableName: String, on: ColumnStatement) extends Join {
    def toStatement = {
      val (onSql, onPlaceHolders) = on.toStatement
      (s"RIGHT JOIN $tableName ON $onSql", onPlaceHolders)
    }
  }

  case class InnerJoin(tableName: String, on: ColumnStatement) extends Join {
    def toStatement = {
      val (onSql, onPlaceHolders) = on.toStatement
      (s"INNER JOIN $tableName ON $onSql", onPlaceHolders)
    }
  }

  case class IncompleteSelect(columnNames: Seq[String]) extends Statement {
    def toStatement: Nothing = throw new Exception("Incomplete Select Statement")
    def from(tableName: String): Select = Select(columnNames, tableName, Vector(), Vector())
  }

  case class Select(columnNames: Seq[String], tableName: String, where: Vector[ColumnStatement], joins: Vector[Join]) extends Statement {
    def from(tableName: String): Select = this.copy(tableName = tableName)
    def where(column: ColumnStatement): Select = this.copy(where = where :+ column)

    def leftJoin(tableName: String, on: ColumnStatement): Select = {
      this.copy(joins = joins :+ LeftJoin(tableName, on))
    }

    def rightJoin(tableName: String, on: ColumnStatement): Select = {
      this.copy(joins = joins :+ RightJoin(tableName, on))
    }

    def innerJoin(tableName: String, on: ColumnStatement): Select = {
      this.copy(joins = joins :+ InnerJoin(tableName, on))
    }

    override def toString = {
      val whereClauses =
        if (where.isEmpty) ""
        else "WHERE " + where.mkString(" AND ")

      s"""
      SELECT ${columnNames.mkString(", ")}
      FROM $tableName
      $whereClauses
      """
    }

    def toStatement = {
      val (whereClauses, wherePlaceHolders) =
        if (where.isEmpty) ("", Vector())
        else {
          val (clauses, placeHolders) = where.map(_.toStatement).unzip
          val sql = "WHERE " + clauses.mkString(" AND ")
          (sql, placeHolders)
        }

      val (joinClauses, joinPlaceHolders) = joins.map(_.toStatement).unzip

      val placeHolders = joinPlaceHolders ++ wherePlaceHolders

      println("Params: [" + placeHolders.mkString(", ") + "]")

      val sql = s"""
      SELECT ${columnNames.mkString(", ")}
      FROM $tableName
      $joinClauses
      $whereClauses
      """

      (sql, placeHolders)
    }
  }

  def select(columnNames: String*) = IncompleteSelect(columnNames)
  def col(columnName: String) = Column(columnName)

  val query2 =
    select("*")
      .from("members")
      .leftJoin("someTable", col("id").eq(col("someTable.id")))
      .where(
        col("id").eq(2)
          .and(
            col("name").like("Bob%")
          )
      )
  //     .groupBy(col("name").asc
  //       .and(
  //         col("id").desc
  //     )
  //   )
}
