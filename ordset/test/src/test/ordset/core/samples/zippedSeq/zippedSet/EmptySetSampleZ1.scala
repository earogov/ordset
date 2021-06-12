package test.ordset.core.samples.zippedSeq.zippedSet

import ordset.core.ZippedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.zippedSeq.OriginalSeqPatchedTest
import test.ordset.core.samples.segmentSeq.ZippedSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class EmptySetSampleZ1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
    with OriginalSeqPatchedTest[Int, D, Boolean, Boolean] {

  override val sample: String = "Z1"

  override val labels: Set[Label] = super.labels + Labels.emptySet

  val firstSeq: TreapOrderedSet[Int, D] = TreapOrderedSet.getFactory.unsafeBuildAsc(
    ArraySeq(10`](`, 20`](`),
    complementary = false,
    domainOps
  )()

  val secondSeq: TreapOrderedSet[Int, D] = TreapOrderedSet.getFactory.unsafeBuildAsc(
    ArraySeq(25`](`),
    complementary = false,
    domainOps
  )()

  // a:
  // X-----f----](------t-----](--------------f--------------X
  //            10            20
  // b:
  // X----------------f--------------](-----------t----------X
  //                                 25
  //
  // a intersection b:
  // X--------------------------f----------------------------X
  //
  override def sequence: ZippedSegmentSeq[Int, D, Boolean, Boolean, Boolean, Any, Any] =
    ZippedOrderedSet.intersection(firstSeq, secondSeq)

  override def firstSeqPatchedCases: Seq[OriginalSeqPatchedTest.TestCase[Int, D, Boolean]] = List(
    OriginalSeqPatchedTest.TestCase(
      labels = Set(Label("A1")),
      bound = 15`[`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(0`](`, 15`](`),
        complementary = true
      ),
      expected =
        (true  forAll x <= 0) ::
        (false forAll x >  0  & x <= 15) ::
        (true  forAll x >  15) ::
        Nil
    )
  )

  override def secondSeqPatchedCases: Seq[OriginalSeqPatchedTest.TestCase[Int, D, Boolean]] = List(
    OriginalSeqPatchedTest.TestCase(
      labels = Set(Label("B1")),
      bound = 15`[`,
      patch = ArrayOrderedSet.unchecked(
        ArraySeq(0`](`, 15`](`),
        complementary = true
      ),
      expected =
        (true  forAll x <= 0) ::
        (false forAll x >  0  & x <= 15) ::
        (true  forAll x >  15) ::
        Nil
    )
  )
}
