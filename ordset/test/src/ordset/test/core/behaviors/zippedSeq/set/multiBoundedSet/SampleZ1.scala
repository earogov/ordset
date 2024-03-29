package ordset.test.core.behaviors.zippedSeq.set.multiBoundedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.random.RngManager
import ordset.test.Label
import ordset.test.Label.*
import ordset.test.core.behaviors.zippedSeq.OriginalSeqPatchTest
import ordset.test.core.samples.segmentSeq.ZippedSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait SampleZ1[D[X] <: Domain[X]]
  extends OriginalSeqPatchTest[Int, D, Boolean, Boolean] {
  self: ZippedSeqSample[Int, D, Boolean, Boolean, Boolean] =>

  override val sample: String = "Z1"

  override lazy val firstSeqPatchCases: Seq[OriginalSeqPatchTest.TestCase[Int, D, Boolean]] = List(
    OriginalSeqPatchTest.TestCase(
      labels = Set(label("A1")),
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
      labels = Set(label("A2")),
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
      labels = Set(label("A3")),
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

  override lazy val secondSeqPatchCases: Seq[OriginalSeqPatchTest.TestCase[Int, D, Boolean]] = List(
    OriginalSeqPatchTest.TestCase(
      labels = Set(label("B1")),
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
      labels = Set(label("B2")),
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
      labels = Set(label("B3")),
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
