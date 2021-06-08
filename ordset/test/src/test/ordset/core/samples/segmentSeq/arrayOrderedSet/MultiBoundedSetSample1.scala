package test.ordset.core.samples.segmentSeq.arrayOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.ArrayOrderedSet
import ordset.util.label.Label
import ordset.random.RngManager
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.language.postfixOps

class MultiBoundedSetSample1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.multiBoundedSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.multiBoundedSeq

  override def sequence: GenSegmentSeq =
    ArrayOrderedSet.getFactory.unsafeBuildAsc(bounds, complementary, domainOps)(domainOps.boundOrd.strictValidation)
}
