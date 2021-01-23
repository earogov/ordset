package test.ordset.core.samples.zippedOrderedSet

import ordset.core.{ArrayOrderedSet, ZippedOrderedSet}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.BoundSyntax._
import ordset.util.label.Label
import test.ordset.core.behaviors._
import test.ordset.core.samples.{Labels, SegmentSeqSample}

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class SingleBoundedSetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with singleBoundedSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.singleBoundedSeq

  override def sequence: GenSegmentSeq =
    // b union c:
    //                          in                                                 out
    // X-----------------------------------------------0](0-----------------------------------------------------X
    //
    // c = a union b:
    //                          in                                                 out
    // X-----------------------------------------------0](0-----------------------------------------------------X
    //
    // b:
    //                          in                                                 out
    // X-----------------------------------------------0](0-----------------------------------------------------X
    //
    // a:
    //                                                 out
    // X--------------------------------------------------------------------------------------------------------X
    ZippedOrderedSet.union(
      // b
      ArrayOrderedSet[Int, D](
        ArraySeq(0 `](`),
        complementary = true),
      // c
      ZippedOrderedSet.union(
        // b
        ArrayOrderedSet[Int, D](
          ArraySeq(0 `](`),
          complementary = true),
        // a
        ArrayOrderedSet[Int, D](
          ArraySeq.empty,
          complementary = false)
      )
    )
}
