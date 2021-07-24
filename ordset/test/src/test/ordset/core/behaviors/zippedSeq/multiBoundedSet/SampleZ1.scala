package test.ordset.core.behaviors.zippedSeq.multiBoundedSet

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

  override def firstSeqPatchCases: Seq[OriginalSeqPatchTest.TestCase[Int, D, Boolean]] = List(
    OriginalSeqPatchTest.TestCase(
      labels = Set(Label("A1")),
      bound = 15`[`,
      patch = UniformOrderedSet.defaultEmpty,
      expected =
        (true  forAll x <= 10) ::
        (false forAll x >  10  & x <= 25) ::
        (true  forAll x >  25  & x <  30) ::
        (false forAll x >= 30) ::
        Nil
    ),
    OriginalSeqPatchTest.TestCase(
      labels = Set(Label("A2")),
      bound = 0`(`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(0`](`, 15`](`),
        complementary = true
      ),
      expected =
        (true  forAll x <= 0) ::
        (false forAll x >  0  & x <  5) ::
        (true  forAll x >= 5  & x <= 10) ::
        (false forAll x >  10 & x <= 20) ::
        (true  forAll x >  20 & x <  30) ::
        (false forAll x >= 30) ::
        Nil
    ),
    OriginalSeqPatchTest.TestCase(
      labels = Set(Label("A3")),
      bound = 35`(`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(35`](`, 45`](`),
        complementary = true
      ),
      expected =
        (true  forAll x <= 10) ::
        (false forAll x >  10  & x <= 20) ::
        (true  forAll x >  20  & x <= 35) ::
        (false forAll x >  35  & x <= 45) ::
        (true  forAll x >  45) ::
        Nil
    )
  )

  override def secondSeqPatchCases: Seq[OriginalSeqPatchTest.TestCase[Int, D, Boolean]] = List(
    OriginalSeqPatchTest.TestCase(
      labels = Set(Label("B1")),
      bound = 15`[`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(17`)[`),
        complementary = true
      ),
      expected =
        (false forAll x <  5) ::
        (true  forAll x >= 5  & x <  17) ::
        (false forAll x >= 17 & x <= 25) ::
        (true  forAll x >  25 & x <= 35) ::
        (false forAll x >  35) ::
        Nil
    ),
    OriginalSeqPatchTest.TestCase(
      labels = Set(Label("B2")),
      bound = 0`(`,
      patch = UniformOrderedSet.defaultUniversal,
      expected =
        (true  forAll x <  15) ::
        (false forAll x >= 15 & x <= 25) ::
        (true  forAll x >  25 & x <= 35) ::
        (false forAll x >  35) ::
        Nil
    ),
    OriginalSeqPatchTest.TestCase(
      labels = Set(Label("B3")),
      bound = 40`(`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(30`)[`),
        complementary = true
      ),
      expected =
        (false forAll x <  5) ::
        (true  forAll x >= 5  & x <  15) ::
        (false forAll x >= 15 & x <= 25) ::
        (true  forAll x >  25 & x <  30) ::
        (false forAll x >= 30) ::
        Nil
    )
  )
}
