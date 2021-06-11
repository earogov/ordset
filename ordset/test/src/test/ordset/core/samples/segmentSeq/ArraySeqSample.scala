package test.ordset.core.samples.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.ArraySegmentSeq
import ordset.random.RngManager

abstract class ArraySeqSample[E, D <: Domain[E], V](
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[E, D, V, ArraySegmentSeq[E, D, V]]
