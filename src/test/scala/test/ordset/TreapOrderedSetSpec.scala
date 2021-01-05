package test.ordset

import ordset.domain.{Domain, DomainOps}
import ordset.util.RandomUtil
import org.scalatest.funspec.AnyFunSpec

import scala.collection.immutable.ArraySeq

class TreapOrderedSetSpec extends AnyFunSpec
  with SegmentSeqBehaviors[Int, Domain[Int], Boolean] {

  import ordset._
  import instances.Int._
  import instances.Boolean._

  import scala.language.postfixOps
  import ordset.syntax.SetBuilderNotation._
  import ordset.syntax.BoundSyntax._

  type Dom = Domain[Int]
  type SegmentSeq = SetSegmentSeq[Int, Dom]
  type TestCase = SegmentSeqTestCase[Int, Dom, Boolean]
  type TestSuite = SegmentSeqTestSuite[Int, Dom, Boolean]

  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]
  val domainOps: DomainOps[Int, Dom] = implicitly[DomainOps[Int, Dom]]

  private val testSuites = List(1, 10, 100, 1000, 10000).map(seed => new TreapSeqTestSuite(seed))

  private class TreapSeqTestSuite(seed: Int) extends TestSuite {

    private val emptyCaseSeed: Int = 1
    private val universalCaseSeed: Int = 2
    private val singleBoundedCaseSeed: Int = 3
    private val multiBoundedCaseSeed: Int = 4
    private val degenerateCaseSeed: Int = 5

    override val emptyCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        makeDescription(Description.emptySet),
      sequence =
        TreapOrderedSet.fromIterable[Int, Dom](
          ArraySeq.empty,
          RandomUtil.intLazyList(seed * emptyCaseSeed),
          complement = false,
          domainOps.boundOrd.validateStrictly
        ),
      expected =
        (false forAll x) :: Nil
    )

    override val universalCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        makeDescription(Description.universalSet),
      sequence =
        TreapOrderedSet.fromIterable[Int, Dom](
          ArraySeq.empty,
          RandomUtil.intLazyList(seed * universalCaseSeed),
          complement = true,
          domainOps.boundOrd.validateStrictly
        ),
      expected =
        (true forAll x) :: Nil
    )

    override val singleBoundedCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        makeDescription(Description.singleBoundedSet),
      sequence =
        TreapOrderedSet.fromIterable[Int, Dom](
          ArraySeq(0 `](`),
          RandomUtil.intLazyList(seed * singleBoundedCaseSeed),
          complement = true,
          domainOps.boundOrd.validateStrictly
        ),
      expected =
        (true forAll x <= 0) ::
        (false forAll x > 0) ::
        Nil
    )

    override val multiBoundedCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        makeDescription(Description.multiBoundedSet),
      sequence =
        TreapOrderedSet.fromIterable[Int, Dom](
          ArraySeq(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`, 50 `](`, 60 `](`, 70 `)[`, 80 `)[`),
          RandomUtil.intLazyList(seed * multiBoundedCaseSeed),
          complement = false,
          domainOps.boundOrd.validateStrictly
        ),
      expected =
        (false forAll x < 0) ::
        (true forAll x >= 0 & x < 10) ::
        (false forAll x >= 10 & x < 20) ::
        (true forAll x >= 20 & x < 30) ::
        (false forAll x >= 30 & x < 40) ::
        (true forAll x >= 40 & x <= 50) ::
        (false forAll x > 50 & x <= 60) ::
        (true forAll x > 60 & x < 70) ::
        (false forAll x >= 70 & x < 80) ::
        (true forAll x >= 80) ::
        Nil
    )

    override val degenerateCase: Option[TestCase] = SegmentSeqTestCase.some(
      description =
        makeDescription(Description.degenerateIntervalSet),
      sequence =
        TreapOrderedSet.fromIterable[Int, Dom](
          ArraySeq(0 `)[`, 0 `](`, 10 `)[`, 20 `)[`, 20 `](`, 30 `)[`),
          RandomUtil.intLazyList(seed * degenerateCaseSeed),
          complement = false,
          domainOps.boundOrd.validateStrictly
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

    private def makeDescription(seqDescr: String): String = s"$seqDescr (seed: $seed)"
  }

  describe(s"Treap based ordered set as a segment sequence") {

    val allCases = testSuites.flatMap(s => s.allCases)

    segmentsSupportMovePrevAndNextForCases(allCases)

    segmentsSupportMoveToFirstAndLastForCases(allCases)

    segmentsHaveNextAndPrevIndicatorsForCases(allCases)

    testSuites.foreach { s =>

      it should behave like segmentsSupportMoveToBound(
        s.emptyCase.get.description,
        s.emptyCase.get.sequence,
        ( 10`)`, false forAll x) ::
        ( 15`[`, false forAll x) ::
        (-10`)`, false forAll x) ::
        (-15`[`, false forAll x) ::
        Nil
      )

      it should behave like segmentsSupportMoveToBound(
        s.universalCase.get.description,
        s.universalCase.get.sequence,
        ( 10`)`, true forAll x) ::
        ( 15`[`, true forAll x) ::
        (-10`)`, true forAll x) ::
        (-15`[`, true forAll x) ::
        Nil
      )

      it should behave like segmentsSupportMoveToBound(
        s.singleBoundedCase.get.description,
        s.singleBoundedCase.get.sequence,
        ( 10`)`, false forAll x >  0) ::
        ( 15`[`, false forAll x >  0) ::
        (-10`)`, true  forAll x <= 0) ::
        (-15`[`, true  forAll x <= 0) ::
        (  0`(`, false forAll x >  0) ::
        (  0`]`, true  forAll x <= 0) ::
        Nil
      )

      it should behave like segmentsSupportMoveToBound(
        s.multiBoundedCase.get.description,
        s.multiBoundedCase.get.sequence,
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
      )

      it should behave like segmentsSupportMoveToBound(
        s.degenerateCase.get.description,
        s.degenerateCase.get.sequence,
        ( 0`]`, true  forAll x >= 0  & x <= 0) ::
        (20`(`, true  forAll x >  20 & x <  30) ::
        (-5`]`, false forAll x <  0) ::
        (45`[`, false forAll x >= 30) ::
        Nil
      )
    }
  }
}