package test.ordset

import ordset.domain.Domain
import org.scalatest.funspec.AnyFunSpec

class ArrayOrderedSetSpec extends AnyFunSpec
  with SegmentSeqCases[Int, Domain[Int], Boolean]
  with SegmentSeqBehaviors[Int, Domain[Int], Boolean] {

  import ordset._
  import scala.collection.immutable.ArraySeq
  import instances.Int._
  import instances.Boolean._

  import scala.language.postfixOps
  import ordset.syntax.SetBuilderNotation._
  import ordset.syntax.BoundSyntax._
  import test.syntax.ArraySyntax._

  type Dom = Domain[Int]
  type SegmentSeq = SetSegmentSeq[Int, Dom]
  
  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]

  override val emptyCase: Option[TestCase] = TestCase.some(
    sequence =
      new ArrayOrderedSet[Int, Dom](ArraySeq.empty, complement = false),
    expected =
      (false forAll x) :: Nil
  )

  override val universalCase: Option[TestCase] = TestCase.some(
    sequence =
      new ArrayOrderedSet[Int, Dom](ArraySeq.empty, complement = true),
    expected =
      (true forAll x) :: Nil
  )

  override val singleBoundedCase: Option[TestCase] = TestCase.some(
    sequence =
      new ArrayOrderedSet[Int, Dom](
        Array(0`](`).toImmutableArraySeq,
        complement = true
      ),
    expected =
      (true forAll x <= 0) ::
      (false forAll x > 0) ::
      Nil
  )

  override val multiBoundedCase: Option[TestCase] = TestCase.some(
    sequence =
      new ArrayOrderedSet[Int, Dom](
        Array(0`)[`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
        complement = false
      ),
    expected =
      (false forAll x <  0) ::
      (true  forAll x >= 0  & x < 10) ::
      (false forAll x >= 10 & x < 20) ::
      (true  forAll x >= 20 & x < 30) ::
      (false forAll x >= 30 & x < 40) ::
      (true  forAll x >= 40) ::
      Nil
  )

  override val degenerateCase: Option[TestCase] = TestCase.some(
    sequence =
      new ArrayOrderedSet[Int, Dom](
        Array(0`)[`, 0`](`, 10`)[`, 20`)[`, 20`](`, 30`)[`).toImmutableArraySeq,
        complement = false
      ),
    expected =
      (false forAll x <  0) ::
      (true  forAll x >= 0  & x <= 0) ::
      (false forAll x >  0  & x <  10) ::
      (true  forAll x >= 10 & x <  20) ::
      (false forAll x >= 20 & x <= 20) ::
      (true  forAll x >  20 & x <  30) ::
      (false forAll x >= 30) ::
      Nil
  )

  describe("Array based ordered set as a segment sequence") {

    it should behave like segmentsSupportMovePrevAndNext(
      "empty set",
      emptyCase.get.sequence,
      emptyCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      "universal set",
      universalCase.get.sequence,
      universalCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      "single bounded set",
      singleBoundedCase.get.sequence,
      singleBoundedCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      "multi bounded set",
      multiBoundedCase.get.sequence,
      multiBoundedCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      "set with degenerate interval",
      degenerateCase.get.sequence,
      degenerateCase.get.expected
    )

    it should behave like segmentsSupportMoveToBound(
      "empty set",
      emptyCase.get.sequence,
      ( 10`)`, false forAll x) ::
      ( 15`[`, false forAll x) ::
      (-10`)`, false forAll x) ::
      (-15`[`, false forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "universal set",
      universalCase.get.sequence,
      ( 10`)`, true forAll x) ::
      ( 15`[`, true forAll x) ::
      (-10`)`, true forAll x) ::
      (-15`[`, true forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "single bounded set",
      singleBoundedCase.get.sequence,
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
      multiBoundedCase.get.sequence,
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
      degenerateCase.get.sequence,
      ( 0`]`, true  forAll x >= 0  & x <= 0) ::
      (20`(`, true  forAll x >  20 & x <  30) ::
      (-5`]`, false forAll x <  0) ::
      (45`[`, false forAll x >= 30) ::
      Nil
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "empty set",
      emptyCase.get.sequence,
      false forAll x,
      false forAll x
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "universal set",
      universalCase.get.sequence,
      true forAll x,
      true forAll x
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "single bounded set",
      singleBoundedCase.get.sequence,
      true forAll x <= 0,
      false forAll x > 0
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "multi bounded set",
      multiBoundedCase.get.sequence,
      false forAll x < 0,
      true forAll x >= 40
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "set with degenerate interval",
      degenerateCase.get.sequence,
      false forAll x <  0,
      false forAll x >= 30
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      "empty set",
      emptyCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      "universal set",
      universalCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      "single bounded set",
      singleBoundedCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      "multi bounded set",
      multiBoundedCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      "set with degenerate interval",
      degenerateCase.get.sequence
    )
  }
}