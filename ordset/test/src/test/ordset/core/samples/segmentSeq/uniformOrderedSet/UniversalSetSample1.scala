package test.ordset.core.samples.segmentSeq.uniformOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.UniformOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

class UniversalSetSample1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.universalSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.universalSet

  override def sequence: GenSegmentSeq = UniformOrderedSet(value = true)
}