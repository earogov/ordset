package ordset

/** Representation of ordered sequence of elements with identifier of type `E`.
  * It's encoded by a sequence of segments which covers universal set without gaps and overlapping.
  * `Boolean` value is assigned to each segment indicating whether it belongs to set:
  *
  *   Segment 0       Segment 1       Segment 2   - segment index
  * -------------|-----------------|------------
  *    false            true           false      - belongs to set
  *
  * These indicators always alternate, so we can keep only the first. It's called `complement`, because its flipping
  * gives complement set. We assume that first segment (with zero index) doesn't belong to set when `complement` = 
  * false and vice versa.
  * Segments are defined by their upper bounds which are stored in `bounds` standard sequence:
  *
  *   Segment 0       Segment 1       Segment 2   - segment index
  *              0                 1              - bound index
  * -------------|-----------------|------------
  *    false            true           false      - belongs to set
  * 
  * Last segment has no upper bound.
  * 
  * Type `W` represents some value which is associated with each segment. 
  * To define ordered set of elements we should consider this value as 'belongs to set' indicator (`W` = `Boolean`).
  * To define ordered map to some type `V` (`E` -> `V`) we assume `W` = `Option[V]`. Where `None` corresponds to
  * segments that don't belong to set.
  */
abstract class AbstractSegmentSeq[E, W] extends SegmentSeq[E, W] {

  override val order: Order[Bound[E]]

  /** @return true if sequence is empty i.e. contains no elements. */
  final override def isEmpty: Boolean = bounds.isEmpty && !complement

  /** @return true if sequence is universal i.e. contains all elements of domain. */
  final override def isUniversal: Boolean = bounds.isEmpty && complement

  /** @return true if sequence contains `bound`. */
  final override def contains(bound: Bound[E]): Boolean = belongsToSet(searchSegmentFromBegin(bound))

  /** @return true if sequence contains `element`. */
  final override def contains(element: E): Boolean = belongsToSet(searchSegmentFromBegin(Bound(element)))

  final override def firstSegment: Segment[E, W] = mkSegment(0)

  final override def lastSegment: Segment[E, W] = mkSegment(lastSegmentIndex)

  /** @return segment containing `bound`. */
  final override def getSegment(bound: Bound[E]): Segment[E, W] = mkMoveFromBegin(bound)

  /** @return segment containing `element`. */
  final override def getSegment(element: E): Segment[E, W] = mkMoveFromBegin(Bound(element))

  /** Sequence of upper bounds of segments. */
  protected val bounds: collection.Seq[Bound.Upper[E]]

  /** Complement indicator. When false first segment doesn't belong to set/map and vice versa.*/
  protected val complement: Boolean

  /**
    * @return value assigned to the segment with index `ind`.
    * For sets it returns 'belongs to set' indicator (`Boolean`).
    * For maps `E` -> `V` it returns `Option[V]` where `None` means that segment doesn't belong to set.
    * @note preconditions:
    *     1. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected def getSegmentValue(ind: Int): W

  /** @return index of segment containing `bound`. */
  protected def searchSegmentFromBegin(bound: Bound[E]): Int

  /** @return index of segment containing `bound`. Search may be optimized depending on current index `ind`. */
  protected def searchSegmentFromIndex(ind: Int, bound: Bound[E]): Int

  /**
    * @return true if segment with index `ind` belongs to set.
    * @note preconditions:
    *     1. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  @inline
  protected final def belongsToSet(ind: Int): Boolean = complement ^ ((ind & 0x00000001) == 0x00000001)

  /**
    * @return interval that corresponds to the segment with index `ind`.
    * @note preconditions:
    *     1. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected final def getInterval(ind: Int): Interval[E] =
    if      (bounds.isEmpty)       Interval.Unbounded
    else if (ind <= 0)             Interval.LeftUnbounded(getUpperBound(0))
    else if (ind >= bounds.length) Interval.RightUnbounded(getLowerBound(lastSegmentIndex))
    else                           Interval.Bounded(getLowerBound(ind), getUpperBound(ind))

  /**
    * @return interval and value that correspond to the segment with index `ind`.
    * @note preconditions:
    *     1. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected final def getIntervalMapping(ind: Int): IntervalMapping[E, W] =
    IntervalMapping(getInterval(ind), getSegmentValue(ind))

  /**
    * @return lower bound of the segment with index `ind`. First segment has no lower bound.
    * @note preconditions:
    *     1. `bounds` is non empty;
    *     2. 1 <= `ind` <= `bounds.length` (last index of segments).
    */
  @inline
  protected final def getLowerBound(ind: Int): Bound.Lower[E] = bounds(ind - 1).flipUpper

  /**
    * @return upper bound of the segment with index `ind`. Last segment has no upper bound.
    * @note preconditions:
    *     1. `bounds` is non empty;
    *     2. 0 <= `ind` < `bounds.length` (last index of segments).
    */
  @inline
  protected final def getUpperBound(ind: Int): Bound.Upper[E] = bounds(ind)

  @inline
  protected final def lastBoundIndex: Int = bounds.length - 1

  @inline
  protected final def lastSegmentIndex: Int = bounds.length

  /**
    * @return segment with index `ind`.
    * @note preconditions:
    *     1. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected final def mkSegment(ind: Int): Segment[E, W] =
    if      (bounds.isEmpty)          Segment.single(getSegmentValue(ind))
    else if (ind <= 0)                Segment.first(getSegmentValue(ind), bounds(ind), mkMoveFromBegin, mkMoveNext(ind))
    else if (ind >= lastSegmentIndex) Segment.last(getSegmentValue(ind), mkMoveFromBegin, mkMovePrev(ind))
    else                              Segment.inner(getSegmentValue(ind), bounds(ind),
                                                    mkMoveFromIndex(ind), mkMovePrev(ind), mkMoveNext(ind))
  /**
    * @return segment (which has previous segment) with index `ind`.
    * @note preconditions:
    *       1. `bounds` is non empty;
    *       2. 1 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected final def mkSegmentWithPrev(ind: Int): Segment.WithPrev[E, W] =
    if (ind >= lastSegmentIndex) Segment.last(getSegmentValue(ind), mkMoveFromBegin, mkMovePrev(ind))
    else                         Segment.inner(getSegmentValue(ind), bounds(ind),
                                               mkMoveFromIndex(ind), mkMovePrev(ind), mkMoveNext(ind))

  /**
    * @return segment (which has next segment) with index `ind`.
    * @note preconditions:
    *       1. `bounds` is non empty;
    *       2. 0 <= `ind` < `bounds.length` (last index of segments).
    */
  protected final def mkSegmentWithNext(ind: Int): Segment.WithNext[E, W] =
    if (ind <= 0) Segment.first(getSegmentValue(ind), bounds(ind), mkMoveFromBegin, mkMoveNext(ind))
    else          Segment.inner(getSegmentValue(ind), bounds(ind),
                                mkMoveFromIndex(ind), mkMovePrev(ind), mkMoveNext(ind))

  /**
    * @param ind - index of current segment.
    * @return function that allows to get segment for specified bound `b`.
    * @note preconditions:
    *       1. `bounds` is non empty;
    *       2. 0 <= `ind` < `bounds.length` (last index of segments).
    */
  protected def mkMoveFromIndex(ind: Int): Bound[E] => Segment[E, W] = b => mkSegment(searchSegmentFromIndex(ind, b))

  /** Function that allows to get segment for specified bound `b`. Search is performed from the beginning. */
  protected def mkMoveFromBegin: Bound[E] => Segment[E, W] = b => mkSegment(searchSegmentFromBegin(b))

  /**
    * @return function that allows to get next segment for current index `ind`.
    * @note preconditions:
    *       1. `bounds` is non empty;
    *       2. 0 <= `ind` < `bounds.length` (last index of segments).
    */
  protected final def mkMoveNext(ind: Int): () => Segment.WithPrev[E, W] = () => mkSegmentWithPrev(ind + 1)

  /**
    * @return function that allows to get previous segment for current index `ind`.
    * @note preconditions:
    *       1. `bounds` is non empty;
    *       2. 1 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected final def mkMovePrev(ind: Int): () => Segment.WithNext[E, W] = () => mkSegmentWithNext(ind - 1)
}