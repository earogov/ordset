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

  type Dom = Domain[Int]
  type SegmentSeq = SetSegmentSeq[Int, Dom]
  
  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]

  override val emptyCase: Option[TestCase] = TestCase.some(
    description =
      "empty set",
    sequence =
      new ArrayOrderedSet[Int, Dom](ArraySeq.empty, complement = false),
    expected =
      (false forAll x) :: Nil
  )

  override val universalCase: Option[TestCase] = TestCase.some(
    description =
      "universal set",
    sequence =
      new ArrayOrderedSet[Int, Dom](ArraySeq.empty, complement = true),
    expected =
      (true forAll x) :: Nil
  )

  override val singleBoundedCase: Option[TestCase] = TestCase.some(
    description =
      "single bounded set",
    sequence =
      new ArrayOrderedSet[Int, Dom](
        ArraySeq(0`](`),
        complement = true
      ),
    expected =
      (true forAll x <= 0) ::
      (false forAll x > 0) ::
      Nil
  )

  override val multiBoundedCase: Option[TestCase] = TestCase.some(
    description =
      "multi bounded set",
    sequence =
      new ArrayOrderedSet[Int, Dom](
        ArraySeq(0`)[`, 10`)[`, 20`)[`, 30`)[`, 40`)[`),
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
    description =
      "set with degenerate interval",
    sequence =
      new ArrayOrderedSet[Int, Dom](
        ArraySeq(0`)[`, 0`](`, 10`)[`, 20`)[`, 20`](`, 30`)[`),
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
      emptyCase.get.description,
      emptyCase.get.sequence,
      emptyCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      universalCase.get.description,
      universalCase.get.sequence,
      universalCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      singleBoundedCase.get.description,
      singleBoundedCase.get.sequence,
      singleBoundedCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      multiBoundedCase.get.description,
      multiBoundedCase.get.sequence,
      multiBoundedCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      degenerateCase.get.description,
      degenerateCase.get.sequence,
      degenerateCase.get.expected
    )

    it should behave like segmentsSupportMoveToBound(
      emptyCase.get.description,
      emptyCase.get.sequence,
      ( 10`)`, false forAll x) ::
      ( 15`[`, false forAll x) ::
      (-10`)`, false forAll x) ::
      (-15`[`, false forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      universalCase.get.description,
      universalCase.get.sequence,
      ( 10`)`, true forAll x) ::
      ( 15`[`, true forAll x) ::
      (-10`)`, true forAll x) ::
      (-15`[`, true forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      singleBoundedCase.get.description,
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
      multiBoundedCase.get.description,
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
      degenerateCase.get.description,
      degenerateCase.get.sequence,
      ( 0`]`, true  forAll x >= 0  & x <= 0) ::
      (20`(`, true  forAll x >  20 & x <  30) ::
      (-5`]`, false forAll x <  0) ::
      (45`[`, false forAll x >= 30) ::
      Nil
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      emptyCase.get.description,
      emptyCase.get.sequence,
      false forAll x,
      false forAll x
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      universalCase.get.description,
      universalCase.get.sequence,
      true forAll x,
      true forAll x
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      singleBoundedCase.get.description,
      singleBoundedCase.get.sequence,
      true forAll x <= 0,
      false forAll x > 0
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      multiBoundedCase.get.description,
      multiBoundedCase.get.sequence,
      false forAll x < 0,
      true forAll x >= 40
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      degenerateCase.get.description,
      degenerateCase.get.sequence,
      false forAll x <  0,
      false forAll x >= 30
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      emptyCase.get.description,
      emptyCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      universalCase.get.description,
      universalCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      singleBoundedCase.get.description,
      singleBoundedCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      multiBoundedCase.get.description,
      multiBoundedCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      degenerateCase.get.description,
      degenerateCase.get.sequence
    )
  }
}