package ordset.test.core.samples.segmentSeq.set.mappedOrderedSet

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.MappedSegmentSeq
import ordset.core.segmentSeq.set.{ArrayOrderedSet, MappedOrderedSet, OrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.random.RngManager
import ordset.util.BooleanUtil
import ordset.util.label.Label
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.MappedSeqSample
import ordset.test.core.Labels

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class EmptySetSample1[D[X] <: Domain[X]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends MappedSeqSample[Int, D, Boolean, Boolean]
  with ordset.test.core.behaviors.segmentSeq.set.emptySet.Sample1[D] {

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
