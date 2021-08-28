package ordset.core.internal

import ordset.core.{Bound, Interval, IntervalRelation, SegmentLikeT, SegmentT, SegmentTruncationT}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps

import scala.collection.{AbstractIterable, AbstractIterator}

/**
 * Segment which maps value of original segment.
 *
 * @tparam E type of element in ordered set
 * @tparam D type of elements domain
 * @tparam U type of original value assigned to interval of elements
 * @tparam V type of new value assigned to interval of elements
 * @tparam S1 type of additional state of original segment
 * @tparam S2 type of additional state of current segment
 */
protected[ordset] trait MappedSegmentLikeT[E, D <: Domain[E], U, V, S1, +S2]
  extends SegmentLikeT[E, D, V, S2]  {

  // Inspection --------------------------------------------------------------- //
  override lazy val value: V = mapFunc(original.value)

  override def self: SegmentT[E, D, V, S2] with MappedSegmentLikeT[E, D, U, V, S1, S2] with S2

  // Protected section -------------------------------------------------------- //
  /**
   * Original segment.
   */
  protected def original: SegmentT[E, D, U, S1]

  /**
   * Function to map segment value.
   */
  protected def mapFunc: U => V
}