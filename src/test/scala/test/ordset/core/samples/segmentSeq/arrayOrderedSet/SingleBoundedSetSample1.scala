package test.ordset.core.samples.segmentSeq.arrayOrderedSet

import ordset.core.ArrayOrderedSet
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.label.Label

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps
import ordset.core.syntax.BoundSyntax._
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

class SingleBoundedSetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.singleBoundedSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.singleBoundedSeq

  override def sequence: GenSegmentSeq =
    ArrayOrderedSet.unchecked(
      ArraySeq(0 `](`),
      complementary = true
    )
}
