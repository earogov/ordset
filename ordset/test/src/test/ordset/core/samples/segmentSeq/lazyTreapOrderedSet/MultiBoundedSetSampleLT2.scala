package test.ordset.core.samples.segmentSeq.lazyTreapOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample

import scala.language.postfixOps

class MultiBoundedSetSampleLT2[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends LazyTreapSeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.lazyTreapSeq.multiBoundedSet.SampleLT2[D] {

  override val labels: Set[Label] = super.labels + Labels.multiBoundedSeq
}
