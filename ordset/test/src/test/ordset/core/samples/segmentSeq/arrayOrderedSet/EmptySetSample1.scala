package test.ordset.core.samples.segmentSeq.arrayOrderedSet

import ordset.core.ArraySegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.ArrayOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.implementations.domain.BoundSelector
import test.ordset.core.samples.segmentSeq.ArraySeqSample

import scala.language.postfixOps

class EmptySetSample1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends ArraySeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.emptySet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.emptySet

  override val sequence: ArraySegmentSeq[Int, D, Boolean] =
    ArrayOrderedSet.getFactory.unsafeBuildAsc(bounds, complementary, domainOps)(domainOps.boundOrd.strictValidation)
}
