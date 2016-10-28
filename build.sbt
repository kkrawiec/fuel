lazy val root = (project in file(".")).
  settings(
    name := "FUEL",
    version := "1.0",
    scalaVersion := "2.11.8",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )