package test.ordset.core.samples.segmentSeq.treapOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

abstract class TreapSegmentSeqSample[E, D <: Domain[E], W](
  seed: Long
)(
  implicit override val domainOps: DomainOps[E, D]
) extends SegmentSeqSample[E, D, W] {

  override def labels: Set[Label] = super.labels + Labels.seed(seed)
}
