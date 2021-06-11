package test.ordset.core.samples.segmentSeq

import ordset.core.UniformSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager

abstract class UniformSeqSample[E, D <: Domain[E], V](
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[E, D, V, UniformSegmentSeq[E, D, V]]
