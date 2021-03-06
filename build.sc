import mill._
import mill.define.Command
import scalalib._

object ordset extends ScalaModule {

  def scalaVersion = "3.0.0-RC1"

  override def scalacOptions = Seq(
    "-language:implicitConversions",
    "-language:higherKinds",
    "-deprecation",
  )

  override def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:2.4.2",
  )

  object test extends Tests {

    def testFrameworks = Seq("org.scalatest.tools.Framework")

    override def moduleDeps = Seq(commonsRandom)

    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.5",
      ivy"org.scalatestplus::junit-4-13:3.2.5.0"
    )

    /**
     * Run one test suite. For example:
     * {{{
     *   mill ordset.test.testOne test.ordset.core.specs.segmentSeq.ArrayOrderedSetSpec
     * }}}
     */
    def testOne(args: String*): Command[Unit] = T.command {
      super.runMain("org.scalatest.run", args: _*)
    }
  }
}

object commonsRandom extends ScalaModule {

  def scalaVersion = "3.0.0-RC1"

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
      ivy"org.scalatest::scalatest:3.2.5",
      ivy"org.scalatestplus::junit-4-13:3.2.5.0"
    )
  }
}