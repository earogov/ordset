package test.ordset.core.samples.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.ZippedSegmentSeq
import ordset.core.SegmentSeq
import ordset.random.RngManager

abstract class ZippedSeqSample[E, D <: Domain[E], U1, U2, V](
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[E, D, V, ZippedSegmentSeq[E, D, U1, U2, V, Any, Any]] {

  val firstSeq: SegmentSeq[E, D, U1]

  val secondSeq: SegmentSeq[E, D, U2]
}
