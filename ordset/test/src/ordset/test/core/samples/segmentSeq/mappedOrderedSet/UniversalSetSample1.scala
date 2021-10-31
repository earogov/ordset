package ordset.test.core.samples.segmentSeq.mappedOrderedSet

import ordset.core.MappedSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{MappedValueOrderedSet, OrderedSet, UniformOrderedSet}
import ordset.core.syntax.BoundSyntax.*
import ordset.random.RngManager
import ordset.util.BooleanUtil
import ordset.util.label.Label
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.samples.segmentSeq.MappedSeqSample
import ordset.test.core.{Labels, TestRngUtil}

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class UniversalSetSample1[D <: Domain[Int]](
  implicit
  override val domainOps: DomainOps[Int, D],
  override val rngManager: RngManager,
  override val boundSelector: BoundSelector[Int]
) extends MappedSeqSample[Int, D, Boolean, Boolean]
  with ordset.test.core.behaviors.segmentSeq.universalSet.Sample1[D] {

  override val labels: Set[Label] = super.labels + Labels.universalSet

  override val originalSeq: OrderedSet[Int, D] =
    UniformOrderedSet.defaultEmpty

  // sequence:
  //                     true
  // X---------------------------------------------X
  //
  // originalSeq:
  //                     false
  // X---------------------------------------------X
  override val sequence: MappedSegmentSeq[Int, D, Boolean, Boolean, Any] =
    MappedValueOrderedSet.inversion(originalSeq)
}
