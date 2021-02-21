package test.ordset.core.behaviors.segmentSeq.multiBoundedSet

import ordset.core.Bound
import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import test.ordset.core.behaviors.segmentSeq.{SegmentMoveToBoundTest, SegmentSeqSlicedTest}
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample3[D <: Domain[Int]]
  extends SegmentMoveToBoundTest[Int, D, Boolean]
  with SegmentSeqSlicedTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean] =>

  override def sample: String = "3"

  override def bounds: IterableOnce[Bound.Upper[Int]] =
    ArraySeq(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`, 50 `](`, 60 `](`, 70 `)[`, 80 `)[`)

  override def complementary: Boolean = false

  override def reference: Seq[GenIntervalRelation] =
    (false forAll x <  0) ::
    (true  forAll x >= 0  & x <  10) ::
    (false forAll x >= 10 & x <  20) ::
    (true  forAll x >= 20 & x <  30) ::
    (false forAll x >= 30 & x <  40) ::
    (true  forAll x >= 40 & x <= 50) ::
    (false forAll x >  50 & x <= 60) ::
    (true  forAll x >  60 & x <  70) ::
    (false forAll x >= 70 & x <  80) ::
    (true  forAll x >= 80) ::
    Nil

  override def moveToBoundSeq: Seq[(GenBound, GenIntervalRelation)] =
    (10`)`, true  forAll x >= 0  & x <  10) ::
    (10`)`, true  forAll x >= 0  & x <  10) ::
    (30`[`, false forAll x >= 30 & x <  40) ::
    (40`)`, false forAll x >= 30 & x <  40) ::
    (40`[`, true  forAll x >= 40 & x <= 50) ::
    (45`[`, true  forAll x >= 40 & x <= 50) ::
    (25`[`, true  forAll x >= 20 & x <  30) ::
    (-5`[`, false forAll x <  0) ::
    (80`[`, true  forAll x >= 80) ::
    (70`)`, true  forAll x >  60 & x <  70) ::
    (75`[`, false forAll x >= 70 & x <  80) ::
    (70`]`, false forAll x >= 70 & x <  80) ::
    (70`[`, false forAll x >= 70 & x <  80) ::
    Nil

  override def slicedCases: Seq[SegmentSeqSlicedTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentSeqSlicedTest.TestCase(
        -10`]`,
        (false forAll x) ::
        Nil,
        reference
      ),
      SegmentSeqSlicedTest.TestCase(
        0`)`,
        (false forAll x) ::
        Nil,
        reference
      ),
      SegmentSeqSlicedTest.TestCase(
        5`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0) ::
        Nil,
        (true  forAll x <  10) ::
        (false forAll x >= 10 & x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60 & x <  70) ::
        (false forAll x >= 70 & x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      SegmentSeqSlicedTest.TestCase(
        20`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <  10) ::
        (false forAll x >= 10 ) ::
        Nil,
        (false forAll x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60 & x <  70) ::
        (false forAll x >= 70 & x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      SegmentSeqSlicedTest.TestCase(
        60`(`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <  10) ::
        (false forAll x >= 10 & x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60) ::
        Nil,
        (true  forAll x <  70) ::
        (false forAll x >= 70 & x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      SegmentSeqSlicedTest.TestCase(
        80`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <  10) ::
        (false forAll x >= 10 & x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60 & x <  70) ::
        (false forAll x >= 70) ::
        Nil,
        (false forAll x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      SegmentSeqSlicedTest.TestCase(
        80`[`,
        reference,
        (true  forAll x) ::
        Nil
      )
    )
}
