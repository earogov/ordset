package ordset.test.core.samples.segmentSeq.arrayOrderedSet

import ordset.core.ArraySegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.ArrayOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.test.core.Labels
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.ArraySeqSample

import scala.language.postfixOps

class UniversalSetSample1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends ArraySeqSample[Int, D, Boolean]
  with ordset.test.core.behaviors.segmentSeq.universalSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.universalSet

  override val sequence: ArraySegmentSeq[Int, D, Boolean] =
    ArrayOrderedSet.getFactory.unsafeBuildAsc(bounds, complementary, domainOps)(domainOps.validation.boundsSeq)
}
