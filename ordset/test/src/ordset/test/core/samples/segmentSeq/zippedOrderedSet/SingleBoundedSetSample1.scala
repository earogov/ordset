package ordset.test.core.samples.segmentSeq.zippedOrderedSet

import ordset.core.ZippedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, OrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.random.RngManager
import ordset.util.label.Label
import ordset.test.core.Labels
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.ZippedSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class SingleBoundedSetSample1[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
  with ordset.test.core.behaviors.segmentSeq.singleBoundedSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.singleBoundedSeq

  override val firstSeq: OrderedSet[Int, D] =
    ArrayOrderedSet.unchecked[Int, D](
      ArraySeq(0 `](`),
      complementary = true
    )

  override val secondSeq: OrderedSet[Int, D] =
    ZippedOrderedSet.union(
      // b
      ArrayOrderedSet.unchecked[Int, D](
        ArraySeq(0 `](`),
        complementary = true
      ),
      // a
      ArrayOrderedSet.unchecked[Int, D](
        ArraySeq.empty,
        complementary = false
      )
    )

  // firstSeq union secondSeq:
  //                          in                                                 out
  // X-----------------------------------------------0](0-----------------------------------------------------X
  //
  // secondSeq = a union b:
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
  override val sequence: ZippedSegmentSeq[Int, D, Boolean, Boolean, Boolean, Any, Any] =
    ZippedOrderedSet.union(firstSeq, secondSeq)
}
