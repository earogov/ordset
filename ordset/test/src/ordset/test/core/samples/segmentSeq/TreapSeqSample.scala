package ordset.test.core.samples.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.*
import ordset.random.RngManager
import ordset.test.Label
import ordset.test.Label.*
import ordset.test.core.implementations.domain.BoundSelector

abstract class TreapSeqSample[E, D[X] <: Domain[X], V](
  seed: Long
)(
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[E]
) extends SegmentSeqSample[E, D, V, TreapSegmentSeq[E, D, V]] {

  override def labels: Set[Label] = super.labels + longSeedLabel(seed)
}
