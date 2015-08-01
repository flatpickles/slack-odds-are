lazy val root = (project in file(".")).
  settings(
    name := "odds_are",
    version := "1.0",
    scalaVersion := "2.11.4"
  )


libraryDependencies += "com.github.gilbertw1" %% "slack-scala-client" % "0.1.1"