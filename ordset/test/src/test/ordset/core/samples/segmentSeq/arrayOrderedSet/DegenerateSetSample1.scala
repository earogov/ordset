package test.ordset.core.samples.segmentSeq.arrayOrderedSet

import ordset.core.ArraySegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.ArrayOrderedSet
import ordset.util.label.Label
import ordset.random.RngManager
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.ArraySeqSample

import scala.language.postfixOps

class DegenerateSetSample1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends ArraySeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.degenerateSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.degenerateSeq

  override val sequence: ArraySegmentSeq[Int, D, Boolean] =
    ArrayOrderedSet.getFactory.unsafeBuildAsc(bounds, complementary, domainOps)()
}