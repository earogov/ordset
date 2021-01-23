package test.ordset.core.samples.zippedOrderedSet

import ordset.core.{ArrayOrderedSet, ZippedOrderedSet}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.BoundSyntax._
import ordset.util.label.Label
import test.ordset.core.behaviors._
import test.ordset.core.samples.{Labels, SegmentSeqSample}

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class DegenerateSetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with degenerateSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.degenerateSeq

  override def sequence: GenSegmentSeq =
    // c intersection d:
    //     out   in        out                in         out         in        out               in
    // X--------0)|(0--------------10)[10--------------20)|(20---------------30)|(30----------------------------X
    //
    // d:
    //                                              in
    // X--------------------------------------------------------------------------------------------------------X
    //
    // c = a union b:
    //     out   in        out                in         out         in        out               in
    // X--------0)|(0--------------10)[10--------------20)|(20---------------30)|(30----------------------------X
    //
    // b:
    //     out   in                           out                                                in
    // X--------0)|(0---------------------------------------------------------30](------------------------------X
    //
    // a:
    //     out   in        out                in         out         in                          out
    // X--------0)|(0--------------10)[10--------------20)|(20---------------30)[30-----------------------------X
    ZippedOrderedSet.intersection(
      // d
      ArrayOrderedSet[Int, D](
        ArraySeq.empty,
        complementary = true),
      // c
      ZippedOrderedSet.union(
        // b
        ArrayOrderedSet[Int, D](
          ArraySeq(0 `)[`, 0 `](`, 30 `](`),
          complementary = false),
        // a
        ArrayOrderedSet[Int, D](
          ArraySeq(0 `)[`, 0 `](`, 10 `)[`, 20 `)[`, 20 `](`, 30 `)[`),
          complementary = false)
      )
    )
}