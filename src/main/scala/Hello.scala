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
  case class Where(parent: Select, columnName: String, operator: String, columnValue: Any) {
    def eq(value: Any) = parent.addWhereClause(this.copy(operator = "=", columnValue = value))
    def gt(value: Any) = parent.addWhereClause(this.copy(operator = ">", columnValue = value))
    def gte(value: Any) = parent.addWhereClause(this.copy(operator = ">=", columnValue = value))
    def lt(value: Any) = parent.addWhereClause(this.copy(operator = "<", columnValue = value))
    def lte(value: Any) = parent.addWhereClause(this.copy(operator = "<=", columnValue = value))

    override def toString = s"$columnName $operator $columnValue"
    def toStatement = (s"$columnName $operator ?", columnValue)
  }

  case class Column(columnName: String) {
    def eq(value: Column) = println("Column")
    def eq(value: Any) = println("Any")
  }
  // case class ColumnStatement()

  case class Select(columnNames: Seq[String], tableName: String, where: Vector[Where]) {
    def from(tableName: String) = this.copy(tableName = tableName)
    def where(columnName: String) = Where(this, columnName, "=", "?")
    def addWhereClause(whereClause: Where): Select = this.copy(where = where :+ whereClause)

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
      val emptyTuple = (Vector.empty[String], Vector.empty[Any])
      val (clauses, params) =
        if (where.isEmpty) emptyTuple
        else {
          where.map(_.toStatement)
            .foldLeft(emptyTuple) {
              case ((clauses, params), (clause, param)) => (clauses :+ clause, params :+ param)
            }
        }

      val whereClauses =
        if (clauses.isEmpty) ""
        else "WHERE " + clauses.mkString(" AND ")

      println("Params: [" + params.mkString(", ") + "]")

      s"""
      SELECT ${columnNames.mkString(", ")}
      FROM $tableName
      $whereClauses
      """
    }
  }

  def select(columnNames: String*) = Select(columnNames, "?", Vector())
  def col(columnName: String) = Column(columnName)

  val query =
    select("*")
      .from("members")
      .where("id").eq(2)

  // val query2 =
  //   select("*")
  //     .from("members")
  //     .leftJoin("someTable", col("id").eq(col("someTable.id")))
  //     .where(
  //       col("id").eq(2)
  //         .and(
  //           col("name").like("Bob%")
  //         )
  //     )
  //     .groupBy(col("name").asc
  //       .and(
  //         col("id").desc
  //     )
  //   )
}
