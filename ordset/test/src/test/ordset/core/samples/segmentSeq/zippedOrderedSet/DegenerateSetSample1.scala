package test.ordset.core.samples.segmentSeq.zippedOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class DegenerateSetSample1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.degenerateSet.Sample1[D] {

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
      ArrayOrderedSet.unchecked[Int, D](
        ArraySeq.empty,
        complementary = true
      ),
      // c
      ZippedOrderedSet.union(
        // b
        ArrayOrderedSet.fromIterableUnsafe[Int, D](
          ArraySeq(0 `)[`, 0 `](`, 30 `](`),
          complementary = false,
          domainOps
        )(),
        // a
        ArrayOrderedSet.fromIterableUnsafe[Int, D](
          ArraySeq(0 `)[`, 0 `](`, 10 `)[`, 20 `)[`, 20 `](`, 30 `)[`),
          complementary = false,
          domainOps
        )()
      )
    )
}