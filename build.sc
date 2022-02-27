import mill._
import scalalib._
import publish._
import define.Command

import $ivy.`com.lihaoyi::mill-contrib-versionfile:`
import mill.contrib.versionfile.VersionFileModule

object versionFile extends VersionFileModule

trait CoreModule extends ScalaModule with PublishModule {

  override def scalaVersion = "3.1.1"

  override def scalacOptions = Seq(
    "-encoding", "utf-8",                 // Specify character encoding used by source files.
    "-deprecation",                       // Emit warning and location for usages of deprecated APIs.
    "-language:implicitConversions",      // Allow definition of implicit functions called views.
    "-language:higherKinds",              // Allow higher-kinded types.
    "-Yexplicit-nulls",                   // Make reference types (anything that extends AnyRef) non-nullable.
    "-Wunused:nowarn"                     // Warn if @nowarn annotation is unused (suppress no warning).
  )

  override def scalaDocOptions = Seq(
    "-comment-syntax", "wiki"
  )

  override def publishVersion = versionFile.currentVersion().toString

  override def pomSettings = PomSettings(
    description = artifactName(),
    organization = "io.github.earogov",
    url = "https://github.com/earogov/ordset",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("earogov", "ordset"),
    developers = Seq(
      Developer("earogov", "Eugene Rogov", "https://github.com/earogov")
    )
  )

  trait TestModule extends Tests {

    override def testFramework = "org.scalatest.tools.Framework"

    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.11",
      ivy"org.scalatestplus::junit-4-13:3.2.11.0"
    )

    /**
     * Run single test suite. For example:
     * {{{
     *   mill ordset.test.single ordset.test.core.specs.segmentSeq.set.ArrayOrderedSetSpec
     * }}}
     */
    def single(args: String*): Command[Unit] = T.command {
      super.runMain("org.scalatest.run", args: _*)
    }
  }
}

object ordset extends CoreModule {

  override def artifactName = "ordset"

  override def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:2.7.0"
  )

  object test extends TestModule {

    override def moduleDeps = Seq(commonsRandom)
  }
}

object commonsRandom extends CoreModule {

  override def artifactName = ordset.artifactName() + "-" + "commonsRandom"

  override def ivyDeps = Agg(
    ivy"org.apache.commons:commons-rng-simple:1.4",
    ivy"org.apache.commons:commons-rng-client-api:1.4"
  )

  override def moduleDeps = Seq(ordset)

  object test extends TestModule
}