package ordset.test.core.samples.segmentSeq

import ordset.core.UniformSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.test.core.implementations.domain.BoundSelector

abstract class UniformSeqSample[E, D <: Domain[E], V](
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[E]
) extends SegmentSeqSample[E, D, V, UniformSegmentSeq[E, D, V]]
