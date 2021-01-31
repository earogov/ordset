package test.ordset.core.samples.segmentSeq.zippedOrderedSet

import ordset.core.{ArrayOrderedSet, ZippedOrderedSet}
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.label.Label
import ordset.core.syntax.BoundSyntax._
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class UniversalSetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.universalSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.universalSet

  override def sequence: GenSegmentSeq =
    // f intersection c:
    //                                              in
    // X--------------------------------------------------------------------------------------------------------X
    //
    // f = b union ~b:
    //                                              in
    // X--------------------------------------------------------------------------------------------------------X
    //
    // ~b:
    //     out             in                out                   in                  out                in
    // X--------0](0--------------10)[10--------------20)[20---------------30)[30--------------40)[40-----------X
    //
    // c = a union b:
    //                                              in
    // X--------------------------------------------------------------------------------------------------------X
    //
    // b:
    //     in             out                in                   out                  in                 out
    // X--------0](0--------------10)[10--------------20)[20---------------30)[30--------------40)[40-----------X
    //
    // a:
    //                                              in
    // X--------------------------------------------------------------------------------------------------------X
    ZippedOrderedSet.intersection(
      // c
      ZippedOrderedSet.union(
        // b
        ArrayOrderedSet.fromIterableUnsafe[Int, D](
          ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
          complementary = false,
          domainOps
        )(),
        // a
        ArrayOrderedSet.unchecked[Int, D](
          ArraySeq.empty,
          complementary = true
        )
      ),
      // f
      ZippedOrderedSet.union(
        // b
        ArrayOrderedSet.fromIterableUnsafe[Int, D](
          ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
          complementary = false,
          domainOps
        )(),
        // ~b
        ArrayOrderedSet.fromIterableUnsafe[Int, D](
          ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
          complementary = true,
          domainOps
        )()
      )
    )
}
