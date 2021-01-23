package test.ordset.core.samples.arrayOrderedSet

import ordset.core.ArrayOrderedSet
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.BoundSyntax._
import ordset.util.label.Label
import test.ordset.core.behaviors._
import test.ordset.core.samples.{Labels, SegmentSeqSample}

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class EmptySetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with emptySet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.emptySet

  override def sequence: GenSegmentSeq =
    ArrayOrderedSet[Int, D](
      ArraySeq.empty,
      complementary = false
    )
}
