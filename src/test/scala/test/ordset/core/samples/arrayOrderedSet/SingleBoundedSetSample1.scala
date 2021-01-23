package test.ordset.core.samples.arrayOrderedSet

import ordset.core.ArrayOrderedSet
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.label.Label
import test.ordset.core.samples.{Labels, SegmentSeqSample}
import test.ordset.core.behaviors._

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps
import ordset.core.syntax.BoundSyntax._

class SingleBoundedSetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with singleBoundedSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.singleBoundedSeq

  override def sequence: GenSegmentSeq =
    ArrayOrderedSet[Int, D](
      ArraySeq(0 `](`),
      complementary = true
    )
}
