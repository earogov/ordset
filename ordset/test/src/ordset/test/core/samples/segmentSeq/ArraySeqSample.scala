package ordset.test.core.samples.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.ArraySegmentSeq
import ordset.random.RngManager
import ordset.test.core.implementations.domain.BoundSelector

abstract class ArraySeqSample[E, D <: Domain[E], V](
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[E]
) extends SegmentSeqSample[E, D, V, ArraySegmentSeq[E, D, V]]