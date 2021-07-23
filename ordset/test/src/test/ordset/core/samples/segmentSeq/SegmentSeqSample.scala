package test.ordset.core.samples.segmentSeq

import ordset.Hash
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.SetBuilderNotation.BoundBuilder
import ordset.core.util.SegmentSeqUtil
import ordset.core.value.ValueOps
import ordset.core.{Bound, IntervalRelation, SegmentSeq}
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels

abstract class SegmentSeqSample[E, D <: Domain[E], V, +SSeq <: SegmentSeq[E, D, V]](
  implicit
  val domainOps: DomainOps[E, D],
  val rngManager: RngManager
) {

  final type GenIntervalRelation = IntervalRelation[E, D, V]

  val x: BoundBuilder[E, D] = BoundBuilder[E, D](domainOps)

  def valueOps: ValueOps[V] = sequence.valueOps

  def intervalRelationHash: Hash[IntervalRelation[E, D, V]] = domainOps.intervalRelationHash(valueOps.valueHash)

  def sample: String

  def labels: Set[Label] = Set.empty + Labels.sample(sample)

  def sequence: SSeq

  def bounds: IterableOnce[Bound.Upper[E]] =
    SegmentSeqUtil.getUpperBoundsIterableFromSegment(sequence.firstSegment, inclusive = true)

  def complementary: Boolean = sequence.firstSegment.isIncluded

  def reference: Seq[IntervalRelation[E, D, V]] = sequence.firstSegment.forwardLazyList.map(_.intervalRelation)

  override def toString: String = Labels.caseShow.show(labels)
}
