package test.ordset.core.samples.segmentSeq.mappedOrderedSet

import ordset.core.MappedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, MappedOrderedSet, OrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.random.RngManager
import ordset.util.BooleanUtil
import ordset.util.label.Label
import test.ordset.core.implementations.domain.BoundSelector
import test.ordset.core.samples.segmentSeq.MappedSeqSample
import test.ordset.core.{Labels, TestRngUtil}

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class EmptySetSample1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends MappedSeqSample[Int, D, Boolean, Boolean]
  with test.ordset.core.behaviors.segmentSeq.emptySet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.emptySet

  override val originalSeq: OrderedSet[Int, D] =
    ArrayOrderedSet.unchecked[Int, D](
      ArraySeq(0 `](`, 10 `)[`, 20 `)[`, 30 `)[`, 40 `)[`),
      complementary = true
    )

  // sequence:
  //                     false
  // X---------------------------------------------X
  //
  // originalSeq:
  //
  //   true    false   true    false   true  false
  // X------](------)[------)[------)[-----)[------X
  //        0       10      20      30     40    
  override val sequence: MappedSegmentSeq[Int, D, Boolean, Boolean, Any] =
    MappedOrderedSet.apply(originalSeq, BooleanUtil.falsePredicate1)
}
