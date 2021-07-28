package test.ordset.core.samples.segmentSeq

import ordset.Hash
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.SetBuilderNotation.BoundBuilder
import ordset.core.util.SegmentSeqUtil
import ordset.core.value.ValueOps
import ordset.core.{Bound, ExtendedBound, IntervalRelation, SegmentSeq}
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels

abstract class SegmentSeqSample[E, D <: Domain[E], V, +SSeq <: SegmentSeq[E, D, V]](
  implicit
  val domainOps: DomainOps[E, D],
  val rngManager: RngManager
) {

  final type GenIntervalRelation = IntervalRelation[E, D, V]

  /**
   * Builder to simplify syntax of bounds description.
   */
  val x: BoundBuilder[E, D] = BoundBuilder[E, D](domainOps)

  /**
   * Typeclass for value specific operations.
   */
  def valueOps: ValueOps[V] = sequence.valueOps

  /**
   * Typeclass for equality checks and hash calculation of [[IntervalRelation]].
   */
  def intervalRelationHash: Hash[IntervalRelation[E, D, V]] = domainOps.intervalRelationHash(valueOps.valueHash)

  /**
   * ID of sequence under test.
   */
  def sample: String

  /**
   * Labels for string representation of [[SegmentSeqSample]] instance.
   */
  def labels: Set[Label] = Set.empty + Labels.sample(sample)

  /**
   * Segment sequence under test.
   */
  def sequence: SSeq

  /**
   * Upper bounds of [[sequence]].
   */
  def bounds: Iterable[Bound.Upper[E]] =
    SegmentSeqUtil.getUpperBoundsIterableFromSegment(sequence.firstSegment, inclusive = true)

  /**
   * Extended upper bounds of [[sequence]].
   *
   * Same as [[bounds]] but with additional last bound equals to [[ExtendedBound.Upper]].
   */
  def extendedBounds: Iterable[ExtendedBound.Upper[E]] = sequence.firstSegment.forwardIterable.map(_.upperExtended)
  
  /**
   * If `true` then first segment is included in ordered set.
   */
  def complementary: Boolean = sequence.firstSegment.isIncluded

  /**
   * Reference list of interval relations that equivalent to [[sequence]].
   */
  def reference: Seq[IntervalRelation[E, D, V]] = sequence.firstSegment.forwardLazyList.map(_.intervalRelation)

  override def toString: String = Labels.caseShow.show(labels)
}
