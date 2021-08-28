package test.ordset.core.samples.segmentSeq

import ordset.core.{SegmentSeq, MappedSegmentSeq}
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import test.ordset.core.implementations.domain.BoundSelector

abstract class MappedSeqSample[E, D <: Domain[E], U, V](
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[E]
) extends SegmentSeqSample[E, D, V, MappedSegmentSeq[E, D, U, V, Any]] {

  val originalSeq: SegmentSeq[E, D, U]
}
