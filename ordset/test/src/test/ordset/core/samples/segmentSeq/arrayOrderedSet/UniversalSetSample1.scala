package test.ordset.core.samples.segmentSeq.arrayOrderedSet

import ordset.core.ArrayOrderedSet
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.language.postfixOps

class UniversalSetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.universalSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.universalSet

  override def sequence: GenSegmentSeq =
    ArrayOrderedSet.fromIterableUnsafe(bounds, complementary, domainOps)()
}