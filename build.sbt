ThisBuild / tlBaseVersion := "0.1" // your current series x.y

ThisBuild / organization := "io.github.moleike"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers ++= List(
  tlGitHubDev("moleike", "Alexandre Moreno")
)

// false by default, set to true to publish to oss.sonatype.org
ThisBuild / tlSonatypeUseLegacyHost := false

val Scala3 = "3.3.0"
ThisBuild / scalaVersion := Scala3 // the default Scala
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-Ykind-projector"
)

lazy val root = project.in(file(".")).aggregate(core)

lazy val core = project
  .in(file("core"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.10.0"
    )
  )

lazy val examples = project
  .in(file("examples"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.typelevel" %% "cats-mtl" % "1.4.0"
    )
  )
  .dependsOn(core)

lazy val mtl = project
  .in(file("mtl"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-mtl" % "1.4.0"
    )
  )
  .dependsOn(core)

addCommandAlias("linter", ";scalafixAll --rules OrganizeImports")
