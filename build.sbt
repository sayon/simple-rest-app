name := "aptuStudents"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "org.reactivemongo" %% "reactivemongo" % "0.10.0",
  "mysql" % "mysql-connector-java" % "5.1.18"
)

play.Project.playScalaSettings
