package test.ordset.core.samples.uniformOrderedSet

import ordset.core.UniformOrderedSet
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.label.Label
import test.ordset.core.behaviors._
import test.ordset.core.samples.{Labels, SegmentSeqSample}

class EmptySetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with emptySet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.emptySet

  override def sequence: GenSegmentSeq = UniformOrderedSet(value = false)
}