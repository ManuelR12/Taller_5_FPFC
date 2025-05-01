ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8" // O puedes actualizar a 2.13.12 o 2.13.13 si quieres

lazy val root = (project in file("."))
  .settings(
    name := "Proyecto_5_FPFC"
  )

scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")

libraryDependencies ++= Seq(
  "com.storm-enroute" %% "scalameter-core" % "0.21", // <-- CORREGIDO: Usar %%
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4", // Versión actualizada opcional
  "org.scalameta" %% "munit" % "0.7.29" % Test // Versión actualizada opcional
)