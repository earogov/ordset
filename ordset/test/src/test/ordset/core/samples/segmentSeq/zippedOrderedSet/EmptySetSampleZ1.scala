package test.ordset.core.samples.segmentSeq.zippedOrderedSet

import ordset.core.ZippedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set._
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.zippedSeq.OriginalSeqPatchTest
import test.ordset.core.samples.segmentSeq.ZippedSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class EmptySetSampleZ1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
    with test.ordset.core.behaviors.zippedSeq.emptySet.SampleZ1[D] {
  
  override val labels: Set[Label] = super.labels + Labels.emptySet

  override val firstSeq: OrderedSet[Int, D] = TreapOrderedSet.getFactory.unsafeBuildAsc(
    ArraySeq(10`](`, 20`](`),
    complementary = false,
    domainOps
  )()

  override val secondSeq: OrderedSet[Int, D] = TreapOrderedSet.getFactory.unsafeBuildAsc(
    ArraySeq(25`](`),
    complementary = false,
    domainOps
  )()

  // firstSeq:
  // X-----f----](------t-----](--------------f--------------X
  //            10            20
  // secondSeq:
  // X----------------f--------------](-----------t----------X
  //                                 25
  //
  // firstSeq intersection secondSeq:
  // X--------------------------f----------------------------X
  //
  override def sequence: ZippedSegmentSeq[Int, D, Boolean, Boolean, Boolean, Any, Any] =
    ZippedOrderedSet.intersection(firstSeq, secondSeq)
}
