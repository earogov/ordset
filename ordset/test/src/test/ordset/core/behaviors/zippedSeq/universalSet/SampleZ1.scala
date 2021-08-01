package test.ordset.core.behaviors.zippedSeq.universalSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.zippedSeq.OriginalSeqPatchTest
import test.ordset.core.samples.segmentSeq.ZippedSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait SampleZ1[D <: Domain[Int]]
  extends OriginalSeqPatchTest[Int, D, Boolean, Boolean] {
  self: ZippedSeqSample[Int, D, Boolean, Boolean, Boolean] =>

  override val sample: String = "Z1"
  
  override lazy val firstSeqPatchCases: Seq[OriginalSeqPatchTest.TestCase[Int, D, Boolean]] = List(
    OriginalSeqPatchTest.TestCase(
      labels = Set(Label("A1")),
      bound = 15 `[`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(0 `](`, 15 `](`),
        complementary = true
      ),
      expected =
        (true forAll x <= 0) ::
        (false forAll x > 0 & x <= 15) ::
        (true forAll x > 15) ::
        Nil
    )
  )

  override lazy val secondSeqPatchCases: Seq[OriginalSeqPatchTest.TestCase[Int, D, Boolean]] = List(
    OriginalSeqPatchTest.TestCase(
      labels = Set(Label("B1")),
      bound = 15 `[`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(0 `](`, 15 `](`),
        complementary = true
      ),
      expected =
        (true forAll x <= 0) ::
        (false forAll x > 0 & x <= 15) ::
        (true forAll x > 15) ::
        Nil
    )
  )
}
