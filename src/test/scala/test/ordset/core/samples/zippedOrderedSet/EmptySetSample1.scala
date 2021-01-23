package test.ordset.core.samples.zippedOrderedSet

import ordset.core.{ArrayOrderedSet, ZippedOrderedSet}
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.label.Label
import test.ordset.core.behaviors._
import test.ordset.core.samples.{Labels, SegmentSeqSample}
import ordset.core.syntax.BoundSyntax._

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class EmptySetSample1[D <: Domain[Int]](
  implicit override val domainOps: DomainOps[Int, D]
) extends SegmentSeqSample[Int, D, Boolean]
  with emptySet.Sample1[D] {

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
        ArrayOrderedSet[Int, D](
          ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
          complementary = false
        ),
        // a
        ArrayOrderedSet[Int, D](
          ArraySeq.empty,
          complementary = false
        )
      ),
      // f
      ZippedOrderedSet.intersection(
        // b
        ArrayOrderedSet[Int, D](
          ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
          complementary = false),
        // ~b
        ArrayOrderedSet[Int, D](
          ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
          complementary = true
        )
      )
    )
}
