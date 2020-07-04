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
abstract class AbstractSimpleSegmentSeq[E, D <: Domain[E],  W] extends SegmentSeq[E, D, W] { seq =>

  /** @return true if sequence is empty i.e. contains no elements. */
  final override def isEmpty: Boolean = bounds.isEmpty && !complement

  /** @return true if sequence is universal i.e. contains all elements of domain. */
  final override def isUniversal: Boolean = bounds.isEmpty && complement

  /** @return true if sequence contains `bound`. */
  final override def contains(bound: Bound[E]): Boolean = belongsToSet(searchSegmentFromBegin(bound))

  /** @return true if sequence contains `element`. */
  final override def contains(element: E): Boolean = belongsToSet(searchSegmentFromBegin(Bound(element)))

  /** @return first segment of sequence. */
  final override def firstSegment: Segment.First[E, D, W] = if (bounds.isEmpty) SingleSegment() else InitialSegment()

  /** @return last segment of sequence. */
  final override def lastSegment: Segment.Last[E, D, W] = if (bounds.isEmpty) SingleSegment() else TerminalSegment()

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
  protected final def makeSegment(ind: Int): Segment[E, D, W] =
    if      (bounds.isEmpty)          SingleSegment()
    else if (ind <= 0)                InitialSegment()
    else if (ind >= lastSegmentIndex) TerminalSegment()
    else                              InnerSegment(ind)

  /**
    * @return segment (which has previous segment) with index `ind`.
    * @note preconditions: 1. `bounds` is non empty;
    *                      2. 1 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected final def makeSegmentWithPrev(ind: Int): Segment.WithPrev[E, D, W] =
    if (ind >= lastSegmentIndex) TerminalSegment() else InnerSegment(ind)

  /**
    * @return segment (which has next segment) with index `ind`.
    * @note preconditions: 1. `bounds` is non empty;
    *                      2. 0 <= `ind` < `bounds.length` (last index of segments).
    */
  protected final def makeSegmentWithNext(ind: Int): Segment.WithNext[E, D, W] =
    if (ind <= 0) InitialSegment() else InnerSegment(ind)

  /**
    * Base trait for non single segments. It has either previous segment or next.
    * @note preconditions: 1. `bounds` is non empty;
    *                      2. 0 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected sealed trait SegmentBase extends SegmentLike[E, D, W] {

    protected val ind: Int

    override def domain: D = seq.domain

    override def value: W = getSegmentValue(ind)

    override def moveToFirst: Segment.First[E, D, W] = InitialSegment()

    override def moveToLast: Segment.Last[E, D, W] = TerminalSegment()

    override def moveTo(bound: Bound[E]): Segment[E, D, W] = makeSegment(searchSegmentFromIndex(ind, bound))
  }

  /**
    * Segment which has next segment.
    * @note preconditions: 1. 0 <= `ind` < `bounds.length` (last index of segments).
    */
  protected sealed trait SegmentWithNext extends SegmentBase with Segment.WithNext[E, D, W] {

    override def upperBound: Bound.Upper[E] = getUpperBound(ind)

    override def moveNext: Segment.WithPrev[E, D, W] = makeSegmentWithPrev(ind + 1)
  }

  /**
    * Segment which has previous segment.
    * @note preconditions: 1. 1 <= `ind` <= `bounds.length` (last index of segments).
    */
  protected sealed trait SegmentWithPrev extends SegmentBase with Segment.WithPrev[E, D, W] {

    override def lowerBound: Bound.Lower[E] = getLowerBound(ind)

    override def movePrev: Segment.WithNext[E, D, W] = makeSegmentWithNext(ind - 1)
  }

  /**
    * Initial segment of sequence.
    * @note preconditions: 1. `bounds` is non empty;
    */
  protected sealed case class InitialSegment() extends SegmentWithNext with Segment.Initial[E, D, W] {

    override val ind: Int = 0
  }

  /**
    * Terminal segment of sequence.
    * @note preconditions: 1. `bounds` is non empty;
    */
  protected sealed case class TerminalSegment() extends SegmentWithPrev with Segment.Terminal[E, D, W] {

    override val ind: Int = lastSegmentIndex
  }

  /**
    * Inner segment of sequence. It must have both previous and next segments.
    * @note preconditions: 1. 1 <= `ind` < `bounds.length` (last index of segments).
    */
  protected sealed case class InnerSegment(override val ind: Int
  ) extends SegmentWithPrev with SegmentWithNext with Segment.Inner[E, D, W]

  protected sealed case class SingleSegment() extends Segment.Single[E, D, W] {

    override def domain: D = seq.domain

    override def value: W = getSegmentValue(0)
  }
}