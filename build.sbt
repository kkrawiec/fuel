lazy val root = (project in file(".")).
  settings(
    name := "FUEL",
    version := "1.0",
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value,
                                "org.scala-lang" % "scala-compiler" % scalaVersion.value)
  )