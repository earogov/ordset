package test.ordset.core.samples.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.TreapSegmentSeq
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels

abstract class TreapSeqSample[E, D <: Domain[E], V](
  seed: Long
)(
  implicit
  override val domainOps: DomainOps[E, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[E, D, V, TreapSegmentSeq[E, D, V]] {

  override def labels: Set[Label] = super.labels + Labels.seed(seed)
}
