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
abstract class AbstractIndexedSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>

  /** @return true if sequence is empty i.e. contains no elements. */
  final override def isEmpty: Boolean = bounds.isEmpty && !complement

  /** @return true if sequence is universal i.e. contains all elements of domain. */
  final override def isUniversal: Boolean = bounds.isEmpty && complement

  /** @return true if sequence contains `bound`. */
  final override def contains(bound: Bound[E]): Boolean = belongsToSet(searchSegmentFromBegin(bound))

  /** @return true if sequence contains `element`. */
  final override def contains(element: E): Boolean = belongsToSet(searchSegmentFromBegin(Bound(element)))

  /** @return first segment of sequence. */
  final override def firstSegment: Segment.First[E, D, W] = if (bounds.isEmpty) IndexedSingleSegment() else IndexedInitialSegment()

  /** @return last segment of sequence. */
  final override def lastSegment: Segment.Last[E, D, W] = if (bounds.isEmpty) IndexedSingleSegment() else IndexedTerminalSegment()

  /** @return segment containing `bound`. */
  final override def getSegment(bound: Bound[E]): Segment[E, D, W] = makeSegment(searchSegmentFromBegin(bound))

  /** @return segment containing `element`. */
  final override def getSegment(element: E): Segment[E, D, W] = makeSegment(searchSegmentFromBegin(Bound(element)))

  /** Sequence of upper bounds of segments. */
  protected val bounds: collection.Seq[Bound.Upper[E]]

  /** Complement indicator. When false first segment doesn't belong to set/map and vice versa.*/
  protected val complement: Boolean

  /**
    * @return value assigned to the segment with index `ind`.
    * For sets it returns 'belongs to set' indicator (`Boolean`).
    * For maps `E` -> `V` it returns `Option[V]` where `None` means that segment doesn't belong to set.
    * @note preconditions: 1. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected def getSegmentValue(ind: Int): W

  /**
    * @return index of segment containing `bound`.
    * @note preconditions: 1. `bounds` is non empty;
    */
  protected def searchSegmentFromBegin(bound: Bound[E]): Int

  /**
    * @return index of segment containing `bound`. Search may be optimized depending on the current index `ind`.
    * @note preconditions: 1. `bounds` is non empty;
    *                      2. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected def searchSegmentFromIndex(ind: Int, bound: Bound[E]): Int

  /**
    * @return true if segment with index `ind` belongs to set.
    * @note preconditions: 1. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  @inline
  protected final def belongsToSet(ind: Int): Boolean = complement ^ ((ind & 0x00000001) == 0x00000001)

  /**
    * @return interval that corresponds to the segment with index `ind`.
    * @note preconditions: 1. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected final def getInterval(ind: Int): Interval[E] =
    if      (bounds.isEmpty)       Interval.Unbounded
    else if (ind <= 0)             Interval.LeftUnbounded(getUpperBound(0))
    else if (ind >= bounds.length) Interval.RightUnbounded(getLowerBound(lastSegmentIndex))
    else                           Interval.Bounded(getLowerBound(ind), getUpperBound(ind))

  /**
    * @return interval and value that correspond to the segment with index `ind`.
    * @note preconditions: 1. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected final def getIntervalMapping(ind: Int): IntervalMapping[E, W] =
    IntervalMapping(getInterval(ind), getSegmentValue(ind))

  /**
    * @return lower bound of the segment with index `ind`. First segment has no lower bound.
    * @note preconditions: 1. `bounds` is non empty;
    *                      2. 1 <= `ind` <= `bounds.length` (last index of segments).
    */
  @inline
  protected final def getLowerBound(ind: Int): Bound.Lower[E] = bounds(ind - 1).flipUpper

  /**
    * @return upper bound of the segment with index `ind`. Last segment has no upper bound.
    * @note preconditions: 1. `bounds` is non empty;
    *                      2. 0 <= `ind` < `bounds.length` (last index of segments).
    */
  @inline
  protected final def getUpperBound(ind: Int): Bound.Upper[E] = bounds(ind)

  @inline
  protected final def lastBoundIndex: Int = bounds.length - 1

  @inline
  protected final def lastSegmentIndex: Int = bounds.length

  /**
    * @return segment with index `ind`.
    * @note preconditions: 1. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected final def makeSegment(ind: Int): GenSegment =
    if      (bounds.isEmpty)          IndexedSingleSegment()
    else if (ind <= 0)                IndexedInitialSegment()
    else if (ind >= lastSegmentIndex) IndexedTerminalSegment()
    else                              IndexedInnerSegment(ind)

  /**
    * @return segment (which has previous segment) with index `ind`.
    * @note preconditions: 1. `bounds` is non empty;
    *                      2. 1 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected final def makeSegmentWithPrev(ind: Int): SegmentWithPrev =
    if (ind >= lastSegmentIndex) IndexedTerminalSegment() else IndexedInnerSegment(ind)

  /**
    * @return segment (which has next segment) with index `ind`.
    * @note preconditions: 1. `bounds` is non empty;
    *                      2. 0 <= `ind` < `bounds.length` (last index of segments).
    */
  protected final def makeSegmentWithNext(ind: Int): SegmentWithNext =
    if (ind <= 0) IndexedInitialSegment() else IndexedInnerSegment(ind)

  /**
    * Base trait for non single segments. It has either previous segment or next.
    * @note preconditions: 1. `bounds` is non empty;
    *                      2. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected sealed trait IndexedSegmentBase extends SegmentLike[E, D, W] {

    protected val ind: Int

    override def domain: D = seq.domain

    override def value: W = getSegmentValue(ind)

    override def moveToFirst: FirstSegment = IndexedInitialSegment()

    override def moveToLast: LastSegment = IndexedTerminalSegment()

    override def moveTo(bound: Bound[E]): GenSegment = makeSegment(searchSegmentFromIndex(ind, bound))
  }

  /**
    * Segment which has next segment.
    * @note preconditions: 1. 0 <= `ind` < `bounds.length` (last index of segments).
    */
  protected sealed trait IndexedSegmentWithNext extends IndexedSegmentBase with SegmentWithNext {

    override def upperBound: Bound.Upper[E] = getUpperBound(ind)

    override def moveNext: SegmentWithPrev = makeSegmentWithPrev(ind + 1)
  }

  /**
    * Segment which has previous segment.
    * @note preconditions: 1. 1 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected sealed trait IndexedSegmentWithPrev extends IndexedSegmentBase with SegmentWithPrev {

    override def lowerBound: Bound.Lower[E] = getLowerBound(ind)

    override def movePrev: SegmentWithNext = makeSegmentWithNext(ind - 1)
  }

  /**
    * Initial segment of sequence.
    * @note preconditions: 1. `bounds` is non empty;
    */
  protected sealed case class IndexedInitialSegment() extends IndexedSegmentWithNext with InitialSegment {

    override val ind: Int = 0
  }

  /**
    * Terminal segment of sequence.
    * @note preconditions: 1. `bounds` is non empty;
    */
  protected sealed case class IndexedTerminalSegment() extends IndexedSegmentWithPrev with TerminalSegment {

    override val ind: Int = lastSegmentIndex
  }

  /**
    * Inner segment of sequence. It must have both previous and next segments.
    * @note preconditions: 1. 1 <= `ind` < `bounds.length` (last index of segments).
    */
  protected sealed case class IndexedInnerSegment(override val ind: Int
  ) extends IndexedSegmentWithPrev with IndexedSegmentWithNext with InnerSegment

  protected sealed case class IndexedSingleSegment() extends SingleSegment {

    override def domain: D = seq.domain

    override def value: W = getSegmentValue(0)
  }
}