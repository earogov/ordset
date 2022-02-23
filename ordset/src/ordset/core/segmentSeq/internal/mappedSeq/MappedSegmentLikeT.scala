package ordset.core.segmentSeq.internal.mappedSeq

import ordset.core.Bound
import ordset.core.segmentSeq.{SegmentLikeT, SegmentT, Segment, SegmentTruncationT}
import ordset.core.interval.{Interval, IntervalRelation}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps

import scala.collection.{AbstractIterable, AbstractIterator}

/**
 * Segment which maps value of original segment.
 *
 * @tparam E type of elements on ordered domain
 * @tparam D type of ordered domain
 * @tparam U type of original value assigned to interval of elements
 * @tparam V type of new value assigned to interval of elements
 * @tparam S1 type of additional state of original segment
 * @tparam S2 type of additional state of current segment
 */
protected[ordset] trait MappedSegmentLikeT[E, D[X] <: Domain[X], U, V, S1, +S2]
  extends SegmentLikeT[E, D, V, S2]  {

  // Inspection --------------------------------------------------------------- //
  override lazy val value: V = segmentMapFunc(original)

  override def self: SegmentT[E, D, V, S2] with MappedSegmentLikeT[E, D, U, V, S1, S2] with S2

  // Protected section -------------------------------------------------------- //
  /**
   * Original segment.
   */
  protected def original: SegmentT[E, D, U, S1]

  /**
   * Function to map segment.
   */
  protected def segmentMapFunc: Segment[E, D, U] => V
}