package ordset.test.core.samples.segmentSeq

import ordset.Hash
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.SetBuilderNotation.BoundBuilder
import ordset.core.segmentSeq.util.SegmentSeqUtil
import ordset.core.segmentSeq.*
import ordset.core.value.ValueOps
import ordset.core.{Bound, ExtendedBound}
import ordset.core.interval.{Interval, IntervalRelation}
import ordset.random.RngManager
import ordset.test.Label
import ordset.test.Label.*
import ordset.test.core.implementations.domain.BoundSelector

abstract class SegmentSeqSample[E, D[X] <: Domain[X], V, +SSeq <: SegmentSeq[E, D, V]](
  implicit
  val domainOps: DomainOps[E, D],
  val rngManager: RngManager,
  val boundSelector: BoundSelector[E]
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
  lazy val intervalRelationHash: Hash[IntervalRelation[E, D, V]] = domainOps.intervalRelations.hash(valueOps.valueHash)

  /**
   * ID of sequence under test.
   */
  def sample: String

  /**
   * Labels for string representation of [[SegmentSeqSample]] instance.
   */
  def labels: Set[Label] = Set.empty + sampleLabel(sample)

  /**
   * Segment sequence under test.
   */
  def sequence: SSeq

  /**
   * Upper bounds of [[sequence]].
   */
  lazy val bounds: Seq[Bound.Upper[E]] = {
    import scala.language.unsafeNulls
    reference.map(rel => rel.interval match
      case int: Interval.BoundedAbove[_, _] => int.upper
      case _ => null
    ).takeWhile(_ != null)
  }

  /**
   * Extended upper bounds of [[sequence]].
   *
   * Same as [[bounds]] but with additional last bound equals to [[ExtendedBound.Upper]].
   */
  lazy val extendedBounds: Seq[ExtendedBound.Upper[E]] = bounds.appended(ExtendedBound.AboveAll)
  
  /**
   * If `true` then first segment is included in ordered set.
   */
  def complementary: Boolean = sequence.firstSegment.isIncluded

  /**
   * Reference list of interval relations that equivalent to [[sequence]].
   */
  def reference: Seq[IntervalRelation[E, D, V]] = sequence.firstSegment.forwardLazyList.map(_.intervalRelation)

  override def toString(): String = showCaseSet(labels)
}
