package ordset.test.core.samples.segmentSeq.arrayOrderedSet

import ordset.core.ArraySegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.ArrayOrderedSet
import ordset.util.label.Label
import ordset.random.RngManager
import ordset.test.core.Labels
import ordset.test.core.samples.segmentSeq.ArraySeqSample
import ordset.test.core.implementations.domain.BoundSelector

import scala.language.postfixOps

class DegenerateSetSample1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends ArraySeqSample[Int, D, Boolean]
  with ordset.test.core.behaviors.segmentSeq.degenerateSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.degenerateSeq

  override val sequence: ArraySegmentSeq[Int, D, Boolean] =
    ArrayOrderedSet.getFactory.unsafeBuildAsc(bounds, complementary, domainOps)()
}