package ordset.test.core.samples.segmentSeq.set.uniformOrderedSet

import ordset.core.UniformSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.UniformOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.test.core.Labels
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.UniformSeqSample

class EmptySetSample1[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends UniformSeqSample[Int, D, Boolean]
  with ordset.test.core.behaviors.segmentSeq.set.emptySet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.emptySet

  override val sequence: UniformSegmentSeq[Int, D, Boolean] = UniformOrderedSet.defaultEmpty
}