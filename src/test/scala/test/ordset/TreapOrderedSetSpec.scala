package test.ordset

import ordset.domain.{Domain, DomainOps}
import ordset.util.RandomUtil
import org.scalatest.funspec.AnyFunSpec

class TreapOrderedSetSpec extends AnyFunSpec
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
        Array(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
        RandomUtil.intLazyList(1),
        complement = false,
        domainOps.boundOrd.validateStrictly
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

  describe("Treap based ordered set as a segment sequence") {

    it should behave like segmentsSupportMovePrevAndNext(
      multiBoundedCase.get.description,
      multiBoundedCase.get.sequence,
      multiBoundedCase.get.expected
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
  }
}