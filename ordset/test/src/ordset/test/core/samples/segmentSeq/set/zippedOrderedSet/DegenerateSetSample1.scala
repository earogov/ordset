package ordset.test.core.samples.segmentSeq.set.zippedOrderedSet

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

class DegenerateSetSample1[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends ZippedSeqSample[Int, D, Boolean, Boolean, Boolean]
  with ordset.test.core.behaviors.segmentSeq.set.degenerateSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.degenerateSeq
  
  override val firstSeq: OrderedSet[Int, D] = ArrayOrderedSet.unchecked[Int, D](
    ArraySeq.empty,
    complementary = true
  )
  
  override val secondSeq: OrderedSet[Int, D] = ZippedOrderedSet.union(
    // b
    ArrayOrderedSet.unchecked[Int, D](
      ArraySeq(0 `)[`, 0 `](`, 30 `](`),
      complementary = false
    ),
    // a
    ArrayOrderedSet.unchecked[Int, D](
      ArraySeq(0 `)[`, 0 `](`, 10 `)[`, 20 `)[`, 20 `](`, 30 `)[`),
      complementary = false
    )
  )

  // firstSeq intersection secondSeq:
  //     out   in        out                in         out         in        out               in
  // X--------0)|(0--------------10)[10--------------20)|(20---------------30)|(30----------------------------X
  //
  // firstSeq:
  //                                              in
  // X--------------------------------------------------------------------------------------------------------X
  //
  // secondSeq = a union b:
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
  override val sequence: ZippedSegmentSeq[Int, D, Boolean, Boolean, Boolean, Any, Any] =
    ZippedOrderedSet.intersection(firstSeq, secondSeq)
}