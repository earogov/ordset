package ordset.test.core.samples.segmentSeq.set.zippedOrderedSet

import ordset.core.segmentSeq.ZippedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.{ArrayOrderedSet, OrderedSet, ZippedOrderedSet}
import ordset.test.Label
import ordset.test.core.SegmentSeqLabels
import ordset.core.syntax.BoundSyntax.*
import ordset.random.RngManager
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.ZippedSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class UniversalSetSample1[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
  with ordset.test.core.behaviors.segmentSeq.set.universalSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + SegmentSeqLabels.universalSet

  override val firstSeq: OrderedSet[Int, D] =
    ZippedOrderedSet.union(
      // b
      ArrayOrderedSet.unchecked[Int, D](
        ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
        complementary = false
      ),
      // a
      ArrayOrderedSet.unchecked[Int, D](
        ArraySeq.empty,
        complementary = true
      )
    )

  override val secondSeq: OrderedSet[Int, D] =
    ZippedOrderedSet.union(
      // b
      ArrayOrderedSet.unchecked[Int, D](
        ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
        complementary = false
      ),
      // ~b
      ArrayOrderedSet.unchecked[Int, D](
        ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
        complementary = true
      )
    )

  // firstSeq intersection secondSeq:
  //                                              in
  // X--------------------------------------------------------------------------------------------------------X
  //
  // secondSeq = b union ~b:
  //                                              in
  // X--------------------------------------------------------------------------------------------------------X
  //
  // ~b:
  //     out             in                out                   in                  out                in
  // X--------0](0--------------10)[10--------------20)[20---------------30)[30--------------40)[40-----------X
  //
  // firstSeq = a union b:
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
  override val sequence: ZippedSegmentSeq[Int, D, Boolean, Boolean, Boolean, Any, Any] =
    ZippedOrderedSet.intersection(firstSeq, secondSeq)
}
