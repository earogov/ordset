package test.ordset.core.samples.segmentSeq.zippedOrderedSet

import ordset.core.{ArrayOrderedSet, ZippedOrderedSet}
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.label.Label
import ordset.core.syntax.BoundSyntax._
import ordset.random.RngManager
import test.ordset.core.Labels
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class EmptySetSample1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager
) extends SegmentSeqSample[Int, D, Boolean]
  with test.ordset.core.behaviors.segmentSeq.emptySet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.emptySet

  override def sequence: GenSegmentSeq =
    // f union c:
    //                                              out
    // X--------------------------------------------------------------------------------------------------------X
    //
    // f = b intersection ~b:
    //                                              out
    // X--------------------------------------------------------------------------------------------------------X
    //
    // ~b:
    //     out             in                out                   in                  out                in
    // X--------0](0--------------10)[10--------------20)[20---------------30)[30--------------40)[40-----------X
    //
    // c = a intersection b:
    //                                              out
    // X--------------------------------------------------------------------------------------------------------X
    //
    // b:
    //     in             out                in                   out                  in                 out
    // X--------0](0--------------10)[10--------------20)[20---------------30)[30--------------40)[40-----------X
    //
    // a:
    //                                              out
    // X--------------------------------------------------------------------------------------------------------X
    ZippedOrderedSet.union(
      ZippedOrderedSet.intersection(
        // b
        ArrayOrderedSet.fromIterableUnsafe[Int, D](
          ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
          complementary = false,
          domainOps
        )(),
        // a
        ArrayOrderedSet.unchecked[Int, D](
          ArraySeq.empty,
          complementary = false
        )
      ),
      // f
      ZippedOrderedSet.intersection(
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
