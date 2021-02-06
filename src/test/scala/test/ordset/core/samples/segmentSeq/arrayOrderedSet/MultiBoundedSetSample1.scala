package test.ordset.core.samples.segmentSeq.arrayOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.BoundSyntax._
import ordset.util.label.Label
import ordset.core.ArrayOrderedSet
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class MultiBoundedSetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.multiBoundedSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.multiBoundedSeq

  override def sequence: GenSegmentSeq =
    ArrayOrderedSet.fromIterableUnsafe(bounds, complementary, domainOps)()
}
