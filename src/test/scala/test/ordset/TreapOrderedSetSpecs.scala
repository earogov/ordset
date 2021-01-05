package test.ordset

import ordset.domain.{Domain, DomainOps}
import ordset.util.RandomUtil
import org.scalatest.funspec.AnyFunSpec

import scala.collection.immutable.ArraySeq

class TreapOrderedSetSpecSeed1 extends TreapOrderedSetSpec(1)
class TreapOrderedSetSpecSeed10 extends TreapOrderedSetSpec(10)
class TreapOrderedSetSpecSeed100 extends TreapOrderedSetSpec(100)
class TreapOrderedSetSpecSeed1000 extends TreapOrderedSetSpec(1000)
class TreapOrderedSetSpecSeed10000 extends TreapOrderedSetSpec(10000)

abstract class TreapOrderedSetSpec(
  val seed: Int
) extends AnyFunSpec
  with SegmentSeqCases[Int, Domain[Int], Boolean]
  with SegmentSeqBehaviors[Int, Domain[Int], Boolean] {

  import ordset._
  import instances.Int._
  import instances.Boolean._

  import scala.language.postfixOps
  import ordset.syntax.SetBuilderNotation._
  import ordset.syntax.BoundSyntax._

  type Dom = Domain[Int]
  type SegmentSeq = SetSegmentSeq[Int, Dom]

  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]
  val domainOps: DomainOps[Int, Dom] = implicitly[DomainOps[Int, Dom]]

  override val multiBoundedCase: Option[TestCase] = TestCase.some(
    description =
      "multi bounded set",
    sequence =
      TreapOrderedSet.fromIterable[Int, Dom](
        ArraySeq(0`)[`, 10`)[`, 20`)[`, 30`)[`, 40`)[`, 50`](`, 60`](`, 70`)[`, 80`)[`),
        RandomUtil.intLazyList(seed),
        complement = false,
        domainOps.boundOrd.validateStrictly
      ),
    expected =
      (false forAll x < 0) ::
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
  )

  describe(s"Treap based ordered set as a segment sequence. Seed: $seed") {

    it should behave like segmentsSupportMovePrevAndNext(
      multiBoundedCase.get.description,
      multiBoundedCase.get.sequence,
      multiBoundedCase.get.expected
    )

    it should behave like segmentsSupportMoveToBound(
      multiBoundedCase.get.description,
      multiBoundedCase.get.sequence,
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

    it should behave like segmentsSupportMoveToFirstAndLast(
      multiBoundedCase.get.description,
      multiBoundedCase.get.sequence,
      false forAll x <  0,
      true  forAll x >= 80
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      multiBoundedCase.get.description,
      multiBoundedCase.get.sequence
    )
  }
}