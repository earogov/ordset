package test.ordset.core.samples.segmentSeq.zippedOrderedSet

import ordset.core.{AbstractZippedSegmentSeq, SegmentSeq}
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

abstract class ZippedSeqSample[E, D <: Domain[E], U1, U2, V](
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[E, D, V]()(
  domainOps, rngManager
) {

  override type GenSegmentSeq = AbstractZippedSegmentSeq[E, D, U1, U2, V, Any, Any]
}
