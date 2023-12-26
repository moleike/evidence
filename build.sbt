lazy val commonSettings = Seq(
  scalaVersion := "3.3.0",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-Ykind-projector"
  )
)

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .aggregate(core)

lazy val core = project
  .in(file("core"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.10.0",
    )
  )

lazy val examples = project
  .in(file("examples"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.typelevel" %% "cats-mtl" % "1.4.0",
    )
  )
  .dependsOn(core)

lazy val mtl = project
  .in(file("mtl"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-mtl" % "1.4.0",
    )
  )
  .dependsOn(core)

