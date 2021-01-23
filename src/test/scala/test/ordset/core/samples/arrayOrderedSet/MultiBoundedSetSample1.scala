package test.ordset.core.samples.arrayOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.BoundSyntax._
import ordset.util.label.Label
import ordset.core.ArrayOrderedSet
import test.ordset.core.samples.{Labels, SegmentSeqSample}
import test.ordset.core.behaviors._
import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class MultiBoundedSetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with multiBoundedSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.multiBoundedSeq

  override def sequence: GenSegmentSeq =
    ArrayOrderedSet[Int, D](
      ArraySeq(0 `)[`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
      complementary = false
    )
}
