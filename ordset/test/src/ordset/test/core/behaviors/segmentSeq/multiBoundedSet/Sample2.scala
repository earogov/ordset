package ordset.test.core.behaviors.segmentSeq.multiBoundedSet

import ordset.core.{Bound, ExtendedBound, SegmentSeq}
import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.test.core.behaviors.TestTuple
import ordset.test.core.behaviors.segmentSeq.{SegmentContainsTest, SegmentMoveToBoundTest, SegmentRestrictBoundTest}
import ordset.test.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample2[D <: Domain[Int]]
  extends SegmentMoveToBoundTest[Int, D, Boolean]
    with SegmentContainsTest[Int, D, Boolean]
    with SegmentRestrictBoundTest[Int, D, Boolean]{
  self: SegmentSeqSample[Int, D, Boolean, SegmentSeq[Int, D, Boolean]] =>

  override val sample: String = "2"
  
  override val complementary: Boolean = true

  override val reference: Seq[GenIntervalRelation] =
    (true  forAll x <= 0) ::
    (false forAll x >  0  & x <  5) ::
    (true  forAll x >= 5  & x <  7) ::
    (false forAll x >= 7  & x <= 20) ::
    (true  forAll x >  20 & x <  25) ::
    (false forAll x >= 25 & x <= 35) ::
    (true  forAll x >  35 & x <  40) ::
    (false forAll x >= 40 & x <  60) ::
    (true  forAll x >= 60) ::
    Nil

  override lazy val moveToBoundCases: Seq[(ExtendedBound[Int], GenIntervalRelation)] =
    ( 7`[`, false forAll x >= 7  & x <= 20) ::
    (30`)`, false forAll x >= 25 & x <= 35) ::
    ( 0`)`, true  forAll x <= 0) ::
    (ExtendedBound.BelowAll, true forAll x <= 0) ::
    (40`]`, false forAll x >= 40 & x <  60) ::
    (40`]`, false forAll x >= 40 & x <  60) ::
    (ExtendedBound.AboveAll, true forAll x >= 60) ::
    (45`[`, false forAll x >= 40 & x <  60) ::
    ( 5`)`, false forAll x >  0  & x <  5 ) ::
    Nil

  override lazy val containsCases: Seq[SegmentContainsTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentContainsTest.TestCase(
        bound = 0`]`,
        includedBounds = List(ExtendedBound.BelowAll, -10`[`, 0`[`, 0`]`, 0`)`),
        excludedBounds = List(ExtendedBound.AboveAll, 10`(`, 0`(`)
      ),
      SegmentContainsTest.TestCase(
        bound = 27`]`,
        includedBounds = List(35`]`, 35`[`, 35`)`),
        excludedBounds = List(35`(`)
      ),
      SegmentContainsTest.TestCase(
        bound = 37`]`,
        includedBounds = List(35`(`, 39`)`),
        excludedBounds = List(35`]`, 35`[`, 35`)`, 40`[`, 40`]`)
      ),
      SegmentContainsTest.TestCase(
        bound = 65`]`,
        includedBounds = List(ExtendedBound.AboveAll, 60`[`, 60`]`, 60`(`, 61`)`),
        excludedBounds = List(ExtendedBound.BelowAll, 60`)`, 59`[`)
      )
    )

  override lazy val restrictCases: Seq[SegmentRestrictBoundTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentRestrictBoundTest.TestCase(
        bound = -10`[`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, ExtendedBound.BelowAll),
          TestTuple(ExtendedBound.AboveAll, 0`]`),
          TestTuple(-10`]`, -10`]`),
          TestTuple(0`]`, 0`]`),
          TestTuple(0`(`, 0`]`),
          TestTuple(10`(`, 0`]`),
        )
      ),
      SegmentRestrictBoundTest.TestCase(
        bound = 10`[`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, 7`[`),
          TestTuple(ExtendedBound.AboveAll, 20`]`),
          TestTuple(-10`]`, 7`[`),
          TestTuple(100`]`, 20`]`),
          TestTuple(7`]`, 7`]`),
          TestTuple(7`[`, 7`[`),
          TestTuple(7`)`, 7`[`),
          TestTuple(20`[`, 20`[`),
          TestTuple(20`]`, 20`]`),
          TestTuple(20`)`, 20`)`),
        )
      ),
      SegmentRestrictBoundTest.TestCase(
        bound = 100`[`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, 60`[`),
          TestTuple(ExtendedBound.AboveAll, ExtendedBound.AboveAll),
          TestTuple(100`(`, 100`(`),
          TestTuple(0`]`, 60`[`),
          TestTuple(60`(`, 60`(`),
          TestTuple(60`)`, 60`[`),
        )
      )
    )
}
