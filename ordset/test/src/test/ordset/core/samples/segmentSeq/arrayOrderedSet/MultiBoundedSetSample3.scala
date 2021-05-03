package test.ordset.core.samples.segmentSeq.arrayOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.ArrayOrderedSet
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.language.postfixOps

class MultiBoundedSetSample3[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.multiBoundedSet.Sample3[D] {

  override def labels: Set[Label] = super.labels + Labels.multiBoundedSeq

  override def sequence: GenSegmentSeq =
    ArrayOrderedSet.fromIterableUnsafe(bounds, complementary, domainOps)()
}
