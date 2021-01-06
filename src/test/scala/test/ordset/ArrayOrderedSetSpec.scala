package test.ordset

import ordset.domain.Domain
import org.scalatest.funspec.AnyFunSpec

class ArrayOrderedSetSpec extends AnyFunSpec
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
  type TestCase = SegmentSeqTestCase[Int, Dom, Boolean]
  
  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]

  private val testSuite = new SegmentSeqTestSuite[Int, Dom, Boolean]() {

    override val emptyCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        Description.emptySet,
      sequence =
        ArrayOrderedSet[Int, Dom](ArraySeq.empty, complement = false),
      expected =
        (false forAll x) :: Nil
    )

    override val universalCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        Description.universalSet,
      sequence =
        ArrayOrderedSet[Int, Dom](ArraySeq.empty, complement = true),
      expected =
        (true forAll x) :: Nil
    )

    override val singleBoundedCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        Description.singleBoundedSet,
      sequence =
        ArrayOrderedSet[Int, Dom](
          ArraySeq(0 `](`),
          complement = true
        ),
      expected =
        (true forAll x <= 0) ::
        (false forAll x > 0) ::
        Nil
    )

    override val multiBoundedCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        Description.multiBoundedSet,
      sequence =
        ArrayOrderedSet[Int, Dom](
          ArraySeq(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
          complement = false
        ),
      expected =
        (false forAll x < 0) ::
        (true forAll x >= 0 & x < 10) ::
        (false forAll x >= 10 & x < 20) ::
        (true forAll x >= 20 & x < 30) ::
        (false forAll x >= 30 & x < 40) ::
        (true forAll x >= 40) ::
        Nil
    )

    override val degenerateCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        Description.degenerateIntervalSet,
      sequence =
        ArrayOrderedSet[Int, Dom](
          ArraySeq(0 `)[`, 0 `](`, 10 `)[`, 20 `)[`, 20 `](`, 30 `)[`),
          complement = false
        ),
      expected =
        (false forAll x < 0) ::
        (true forAll x >= 0 & x <= 0) ::
        (false forAll x > 0 & x < 10) ::
        (true forAll x >= 10 & x < 20) ::
        (false forAll x >= 20 & x <= 20) ::
        (true forAll x > 20 & x < 30) ::
        (false forAll x >= 30) ::
        Nil
    )
  }

  describe("Array based ordered set as a segment sequence") {

    val allCases = testSuite.allCases

    segmentsSupportMovePrevAndNextForCases(allCases)

    segmentsSupportMoveToFirstAndLastForCases(allCases)

    segmentsHaveNextAndPrevIndicatorsForCases(allCases)

    it should behave like segmentsSupportMoveToBound(
      testSuite.emptyCase.get.description,
      testSuite.emptyCase.get.sequence,
      ( 10`)`, false forAll x) ::
      ( 15`[`, false forAll x) ::
      (-10`)`, false forAll x) ::
      (-15`[`, false forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      testSuite.universalCase.get.description,
      testSuite.universalCase.get.sequence,
      ( 10`)`, true forAll x) ::
      ( 15`[`, true forAll x) ::
      (-10`)`, true forAll x) ::
      (-15`[`, true forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      testSuite.singleBoundedCase.get.description,
      testSuite.singleBoundedCase.get.sequence,
      ( 10`)`, false forAll x >  0) ::
      ( 15`[`, false forAll x >  0) ::
      (-10`)`, true  forAll x <= 0) ::
      (-15`[`, true  forAll x <= 0) ::
      (  0`(`, false forAll x >  0) ::
      (  0`]`, true  forAll x <= 0) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      testSuite.multiBoundedCase.get.description,
      testSuite.multiBoundedCase.get.sequence,
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
      testSuite.degenerateCase.get.description,
      testSuite.degenerateCase.get.sequence,
      ( 0`]`, true  forAll x >= 0  & x <= 0) ::
      (20`(`, true  forAll x >  20 & x <  30) ::
      (-5`]`, false forAll x <  0) ::
      (45`[`, false forAll x >= 30) ::
      Nil
    )
  }
}