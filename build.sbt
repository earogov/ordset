name := "Ordset"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-Ymacro-annotations",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-deprecation",
  "-explaintypes")
  //"-Xlog-implicits"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  "org.typelevel" %% "cats-core" % "2.1.1",
  "org.typelevel" %% "cats-collections-core" % "0.9.0"
  //"com.chuusai" %% "shapeless" % "2.3.3"
)

Global / cancelable := true
