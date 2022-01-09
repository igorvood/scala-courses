course := "parprog1"
assignment := "scalashop"

scalaVersion := "3.0.0"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies ++= Seq(
  ("com.storm-enroute" %% "scalameter-core" % "0.21").cross(CrossVersion.for3Use2_13),
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3",
  "org.scalameta" %% "munit" % "0.7.26" % Test/*,
  "org.scalacheck" %% "scalacheck" % "1.14.2" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test*/
)

testFrameworks += new TestFramework("munit.Framework")
//testFrameworks +=Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")