package test.ordset

import ordset.domain.Domain
import org.scalatest.funspec.AnyFunSpec

class UniformOrderedSetSpec extends AnyFunSpec
  with SegmentSeqBehaviors[Int, Domain[Int], Boolean] {

  import ordset._
  import instances.Boolean._
  import instances.Int._
  import ordset.syntax.BoundSyntax._
  import ordset.syntax.SetBuilderNotation._

  import scala.language.postfixOps

  type Dom = Domain[Int]
  type SegmentSeq = SetSegmentSeq[Int, Dom]
  type TestCase = SegmentSeqTestCase[Int, Dom, Boolean]
  
  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]

  private val testSuite = new SegmentSeqTestSuite[Int, Dom, Boolean]() {

    override val emptyCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        Description.emptySet,
      sequence =
        UniformOrderedSet.empty,
      expected =
        (false forAll x) :: Nil
    )

    override val universalCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        Description.universalSet,
      sequence =
        UniformOrderedSet.universal,
      expected =
        (true forAll x) :: Nil
    )
  }

  describe("Uniform ordered set as a segment sequence") {

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
  }
}