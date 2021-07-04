package ordset.core.internal

import ordset.core.{Bound, Interval, IntervalRelation, SegmentLikeT, SegmentT}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps

/**
 * Non-sealed superclass of [[MappedSegmentT]].
 *
 * @tparam E type of element in ordered set
 * @tparam D type of elements domain
 * @tparam U type of original value assigned to interval of elements
 * @tparam V type of new value assigned to interval of elements
 * @tparam S1 type of additional state of original segment
 * @tparam S2 type of additional state of current segment
 */
trait MappedSegmentLikeT[E, D <: Domain[E], U, V, S1, +S2] extends SegmentLikeT[E, D, V, S2] {

  // Inspection --------------------------------------------------------------- //
  override lazy val value: V = mapFunc.apply(original.value)

  override def forwardIterable: Iterable[SegmentT[E, D, V, S2] with S2] = original.forwardIterable.map(cons)

  override def forwardIterator: Iterator[SegmentT[E, D, V, S2] with S2] = original.forwardIterator.map(cons)

  override def backwardIterable: Iterable[SegmentT[E, D, V, S2] with S2] = original.backwardIterable.map(cons)

  override def backwardIterator: Iterator[SegmentT[E, D, V, S2] with S2] = original.backwardIterator.map(cons)

  override def forwardLazyList: LazyList[SegmentT[E, D, V, S2] with S2] = original.forwardLazyList.map(cons)

  override def backwardLazyList: LazyList[SegmentT[E, D, V, S2] with S2] = original.backwardLazyList.map(cons)

  // Protected section -------------------------------------------------------- //
  /**
   * Original segment.
   */
  protected def original: SegmentT[E, D, U, S1]

  /**
   * Function to map segment value.
   */
  protected def mapFunc: U => V

  /**
   * Creates new mapped segment applying [[mapFunc]] to input `segment`.
   */
  protected def cons(segment: SegmentT[E, D, U, S1]): SegmentT[E, D, V, S2] with S2
  
  /**
   * Creates new mapped segment which has next segment applying [[mapFunc]] to input `segment`.
   */
  protected def consWithNext(segment: SegmentT.WithNext[E, D, U, S1]): SegmentT.WithNext[E, D, V, S2] with S2

  /**
   * Creates new mapped segment which has previous segment applying [[mapFunc]] to input `segment`.
   */
  protected def consWithPrev(segment: SegmentT.WithPrev[E, D, U, S1]): SegmentT.WithPrev[E, D, V, S2] with S2
}