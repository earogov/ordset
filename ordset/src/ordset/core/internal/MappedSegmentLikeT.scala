package ordset.core.internal

import ordset.core.{Bound, Interval, IntervalRelation, SegmentLikeT, SegmentT, SegmentTruncationT}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps

import scala.collection.{AbstractIterable, AbstractIterator}

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
protected[ordset] trait MappedSegmentLikeT[E, D <: Domain[E], U, V, S1, +S2] extends SegmentLikeT[E, D, V, S2] {

  // Inspection --------------------------------------------------------------- //
  override lazy val value: V = mapFunc.apply(original.value)

  override def self: SegmentT[E, D, V, S2] with MappedSegmentLikeT[E, D, U, V, S1, S2] with S2

  // Navigation --------------------------------------------------------------- //
  override def forwardIterable: Iterable[SegmentT[E, D, V, S2] with S2] = new AbstractIterable {

    override def iterator: Iterator[SegmentT[E, D, V, S2] with S2] = forwardIterator
  }

  override def forwardIterator: Iterator[SegmentT[E, D, V, S2] with S2] = new AbstractIterator {

    private var current: SegmentT[E, D, V, S2] with MappedSegmentLikeT[E, D, U, V, S1, S2] with S2 = _

    override def hasNext: Boolean = current == null || !current.isLast

    override def next(): SegmentT[E, D, V, S2] with S2 =
      if (current == null) {
        current = self
        current
      } else current.original match {
        case s: SegmentT.WithNext[E, D, U, S1] =>
          val nextOriginal = s.moveNext
          current = consWithPrev(nextOriginal.truncation(nextOriginal.lowerBound))
          current
        case _ =>
          SegmentSeqExceptionUtil.throwNoNextSegment(current)
      }
  }

  override def backwardIterable: Iterable[SegmentT[E, D, V, S2] with S2] = new AbstractIterable {

    override def iterator: Iterator[SegmentT[E, D, V, S2] with S2] = backwardIterator
  }

  override def backwardIterator: Iterator[SegmentT[E, D, V, S2] with S2] = new AbstractIterator {

    private var current: SegmentT[E, D, V, S2] with MappedSegmentLikeT[E, D, U, V, S1, S2] with S2 = _

    override def hasNext: Boolean = current == null || !current.isFirst

    override def next(): SegmentT[E, D, V, S2] with S2 =
      if (current == null) {
        current = self
        current
      } else current.original match {
        case s: SegmentT.WithPrev[E, D, U, S1] =>
          val prevOriginal = s.movePrev
          current = consWithNext(prevOriginal.truncation(prevOriginal.upperBound))
          current
        case _ =>
          SegmentSeqExceptionUtil.throwNoPrevSegment(current)
      }
  }

  override def forwardLazyList: LazyList[SegmentT[E, D, V, S2] with S2]

  override def backwardLazyList: LazyList[SegmentT[E, D, V, S2] with S2]

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
   * Creates new mapped segment applying [[mapFunc]] to the segment of input `truncation`.
   */
  protected def cons(
    truncation: SegmentTruncationT[E, D, U, S1, SegmentT[E, D, U, S1]]
  ): SegmentT[E, D, V, S2] with MappedSegmentLikeT[E, D, U, V, S1, S2] with S2

  /**
   * Creates new mapped segment which has next segment applying [[mapFunc]] to the segment of input `truncation`.
   */
  protected def consWithNext(
    truncation: SegmentTruncationT[E, D, U, S1, SegmentT.WithNext[E, D, U, S1]]
  ): SegmentT.WithNext[E, D, V, S2] with MappedSegmentLikeT[E, D, U, V, S1, S2] with S2

  /**
   * Creates new mapped segment which has previous segment applying [[mapFunc]] to the segment of input `truncation`.
   */
  protected def consWithPrev(
    truncation: SegmentTruncationT[E, D, U, S1, SegmentT.WithPrev[E, D, U, S1]]
  ): SegmentT.WithPrev[E, D, V, S2] with MappedSegmentLikeT[E, D, U, V, S1, S2] with S2
}