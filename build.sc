import mill._
import scalalib._

object ordset extends ScalaModule {

  def scalaVersion = "2.13.4"

  override def scalacOptions = Seq(
    "-Ymacro-annotations",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-deprecation",
    "-explaintypes",
    "-opt:l:method",
    "-Xlog-implicits",
    //"-Xprint:typer"
  )

  override def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:2.0.0",
    ivy"org.typelevel::cats-collections-core:0.9.0"
  )

  object test extends Tests {

    def testFrameworks = Seq("org.scalatest.tools.Framework")

    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.1.1"
    )
  }
}

object commonsRandom extends ScalaModule {

  def scalaVersion = "2.13.4"

  override def scalacOptions = Seq(
    "-language:implicitConversions",
    "-language:higherKinds",
    "-deprecation"
  )

  override def ivyDeps = Agg(
    ivy"org.apache.commons:commons-rng-simple:1.3",
    ivy"org.apache.commons:commons-rng-client-api:1.3"
  )

  override def moduleDeps = Seq(ordset)

  object test extends Tests {

    def testFrameworks = Seq("org.scalatest.tools.Framework")

    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.1.1"
    )
  }
}