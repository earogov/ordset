package test.ordset

import ordset.ContinuousDomain
import org.scalatest.funspec.AnyFunSpec

class ArrayOrderedSetSpec extends AnyFunSpec
  with SegmentSeqCases[Int, ContinuousDomain[Int], Boolean]
  with SegmentSeqBehaviors[Int, ContinuousDomain[Int], Boolean] {

  import ordset._
  import scala.collection.immutable.ArraySeq
  import OrderWithDir._

  import scala.language.postfixOps
  import ordset.syntax.SetBuilderNotation._
  import ordset.syntax.BoundSyntax._
  import test.syntax.ArraySyntax._

  type Domain = ContinuousDomain[Int]
  type SegmentSeq = SetSegmentSeq[Int, Domain]

  implicit val domain: Domain = ContinuousDomain()

  override val emptyCase: Option[SegmentSeq] = Some(
    new ArrayOrderedSet[Int, Domain](ArraySeq.empty, complement = false)
  )

  override val universalCase: Option[SegmentSeq] = Some(
    new ArrayOrderedSet[Int, Domain](ArraySeq.empty, complement = true)
  )

  override val singleBoundedCase: Option[SegmentSeq] = Some(
    new ArrayOrderedSet[Int, Domain](Array(0`](`).toImmutableArraySeq, complement = true)
  )

  override val multiBoundedCase: Option[SegmentSeq] = Some(
    new ArrayOrderedSet[Int, Domain](Array(0`)[`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
                                     complement = false)
  )

  override val degenerateCase: Option[SegmentSeq] = Some(
    new ArrayOrderedSet[Int, Domain](Array(0`)[`, 0`](`, 10`)[`, 20`)[`, 20`](`, 30`)[`).toImmutableArraySeq,
                                     complement = false)
  )

  describe("Array based ordered set as a segment sequence") {

    it should behave like segmentsSupportMovePrevAndNext(
      "empty set",
      emptyCase.get.firstSegment,
      (false forAll x) :: Nil
    )

    it should behave like segmentsSupportMovePrevAndNext(
      "universal set",
      universalCase.get.firstSegment,
      (true forAll x) :: Nil
    )

    it should behave like segmentsSupportMovePrevAndNext(
      "single bounded set",
      singleBoundedCase.get.firstSegment,
      (true forAll x <=  0) ::
      (false forAll x > 0) ::
      Nil
    )

    it should behave like segmentsSupportMovePrevAndNext(
      "multi bounded set",
      multiBoundedCase.get.firstSegment,
      (false forAll x <  0) ::
      (true  forAll x >= 0  & x < 10) ::
      (false forAll x >= 10 & x < 20) ::
      (true  forAll x >= 20 & x < 30) ::
      (false forAll x >= 30 & x < 40) ::
      (true  forAll x >= 40) ::
      Nil
    )

    it should behave like segmentsSupportMovePrevAndNext(
      "set with degenerate interval",
      degenerateCase.get.firstSegment,
      (false forAll x <  0) ::
      (true  forAll x >= 0  & x <= 0) ::
      (false forAll x >  0  & x <  10) ::
      (true  forAll x >= 10 & x <  20) ::
      (false forAll x >= 20 & x <= 20) ::
      (true  forAll x >  20 & x <  30) ::
      (false forAll x >= 30) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "empty set",
      emptyCase.get.firstSegment,
      ( 10`)`, false forAll x) ::
      ( 15`[`, false forAll x) ::
      (-10`)`, false forAll x) ::
      (-15`[`, false forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "universal set",
      universalCase.get.firstSegment,
      ( 10`)`, true forAll x) ::
      ( 15`[`, true forAll x) ::
      (-10`)`, true forAll x) ::
      (-15`[`, true forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "single bounded set",
      singleBoundedCase.get.firstSegment,
      ( 10`)`, false forAll x >  0) ::
      ( 15`[`, false forAll x >  0) ::
      (-10`)`, true  forAll x <= 0) ::
      (-15`[`, true  forAll x <= 0) ::
      (  0`(`, false forAll x >  0) ::
      (  0`]`, true  forAll x <= 0) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "multi bounded set",
      multiBoundedCase.get.firstSegment,
      (10`)`, true  forAll x >= 0  & x < 10) ::
      (10`)`, true  forAll x >= 0  & x < 10) ::
      (30`[`, false forAll x >= 30 & x < 40) ::
      (40`)`, false forAll x >= 30 & x < 40) ::
      (40`[`, true  forAll x >= 40) ::
      (45`[`, true  forAll x >= 40) ::
      (25`[`, true  forAll x >= 20 & x < 30) ::
      (-5`[`, false forAll x <  0) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "set with degenerate interval",
      degenerateCase.get.firstSegment,
      ( 0`]`, true  forAll x >= 0  & x <= 0) ::
      (20`(`, true  forAll x >  20 & x <  30) ::
      (-5`]`, false forAll x <  0) ::
      (45`[`, false forAll x >= 30) ::
      Nil
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "empty set",
      emptyCase.get,
      false forAll x,
      false forAll x
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "universal set",
      universalCase.get,
      true forAll x,
      true forAll x
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "single bounded set",
      singleBoundedCase.get,
      true forAll x <=  0,
      false forAll x > 0
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "multi bounded set",
      multiBoundedCase.get,
      false forAll x <  0,
      true forAll x >= 40
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "set with degenerate interval",
      degenerateCase.get,
      false forAll x <  0,
      false forAll x >= 30
    )

    it should behave like segmentsHaveNextAndPrevIndicators("empty set", emptyCase.get)

    it should behave like segmentsHaveNextAndPrevIndicators("universal set", universalCase.get)

    it should behave like segmentsHaveNextAndPrevIndicators("single bounded set", singleBoundedCase.get)

    it should behave like segmentsHaveNextAndPrevIndicators("multi bounded set", multiBoundedCase.get)

    it should behave like segmentsHaveNextAndPrevIndicators("set with degenerate interval", degenerateCase.get)
  }
}