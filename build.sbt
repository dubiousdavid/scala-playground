lazy val root = (project in file(".")).
  settings(
    name := "playground",
    version := "1.0",
    scalaVersion := "2.11.7"
  ).
  settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.6" % "test",
      "org.scalikejdbc" %% "scalikejdbc"       % "2.3.5",
      "org.scalikejdbc" %% "scalikejdbc-jsr310" % "2.3.5",
      "com.h2database"  %  "h2"                % "1.4.191",
      "ch.qos.logback"  %  "logback-classic"   % "1.1.3"
    )
  )
