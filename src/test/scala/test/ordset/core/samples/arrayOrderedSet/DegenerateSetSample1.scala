package test.ordset.core.samples.arrayOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.BoundSyntax._
import ordset.util.label.Label
import ordset.core.ArrayOrderedSet
import test.ordset.core.samples.{Labels, SegmentSeqSample}
import test.ordset.core.behaviors._
import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class DegenerateSetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with degenerateSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.degenerateSeq

  override def sequence: GenSegmentSeq =
    ArrayOrderedSet[Int, D](
      ArraySeq(0 `)[`, 0 `](`, 10 `)[`, 20 `)[`, 20 `](`, 30 `)[`, 30 `](`),
      complementary = false
    )
}