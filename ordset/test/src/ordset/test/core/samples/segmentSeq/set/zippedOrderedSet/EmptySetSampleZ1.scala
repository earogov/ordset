package ordset.test.core.samples.segmentSeq.set.zippedOrderedSet

import ordset.core.segmentSeq.ZippedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.*
import ordset.core.syntax.BoundSyntax.*
import ordset.core.syntax.SetBuilderNotation.*
import ordset.random.RngManager
import ordset.test.Label
import ordset.test.core.SegmentSeqLabels
import ordset.test.core.behaviors.zippedSeq.OriginalSeqPatchTest
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.ZippedSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class EmptySetSampleZ1[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
    with ordset.test.core.behaviors.zippedSeq.set.emptySet.SampleZ1[D] {
  
  override val labels: Set[Label] = super.labels + SegmentSeqLabels.emptySet

  override val firstSeq: OrderedSet[Int, D] = TreapOrderedSet.getFactory.unsafeBuild(
    ArraySeq(10`](`, 20`](`),
    complementary = false
  )

  override val secondSeq: OrderedSet[Int, D] = TreapOrderedSet.getFactory.unsafeBuild(
    ArraySeq(25`](`),
    complementary = false
  )

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
