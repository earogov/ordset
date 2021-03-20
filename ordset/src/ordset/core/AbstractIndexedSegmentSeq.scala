package ordset.core

import ordset.core.domain.{Domain, DomainOps}

import scala.collection.Seq

/**
 * For common description of segment sequence see [[SegmentSeq]].
 *
 * <u>Class is not intended to model empty and universal sets.</u>
 * For such cases implementation based on [[AbstractUniformSegmentSeq]] can be used.
 *
 * Upper bounds of segments are stored in `bounds` standard collection:
 * {{{
 *
 *   Segment 0       Segment 1       Segment 2   - segment index
 *              0                 1              - bound index
 * -------------|-----------------|------------
 *       A               B              C        - value
 * }}}
 * Upper bound of last segment is not stored.
 *
 * <tr>`bounds` collection MUST be non empty.                   </tr>
 * <tr>`bounds` collection SHOULD provide fast access by index. </tr>
 */
abstract class AbstractIndexedSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>

  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = false

  final override def isUniversal: Boolean = false

  final override def isUniform: Boolean = false

  final override def contains(bound: Bound[E]): Boolean = isIncludedInSet(searchSegmentFromBegin(bound))

  final override def contains(element: E): Boolean = super.contains(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = bounds

  final override def firstSegment: IndexedInitialSegment = IndexedInitialSegment()

  final override def lastSegment: IndexedTerminalSegment = IndexedTerminalSegment()

  final override def getSegment(bound: Bound[E]): IndexedSegmentBase with GenSegment =
    makeSegment(searchSegmentFromBegin(bound))

  final override def getSegment(element: E): IndexedSegmentBase with GenSegment =
    getSegment(Bound.Upper.inclusive(element))

  // Protected section -------------------------------------------------------- //
  /**
   * Collection of segments upper bounds.
   *
   * It MUST be non empty.
   */
  protected val bounds: Seq[Bound.Upper[E]]

  /** When false, first segment doesn't belong to set and vice versa.*/
  protected val complementary: Boolean

  /**
   * Validate state after initialization.
   */
  @throws[AssertionError]("if bounds collection is empty")
  protected def validate(): Unit = {
    if (bounds.isEmpty) throw new AssertionError("Bounds collection must be non empty.")
  }

  /**
    * Preconditions:
    *
    * 1. `0 <= ind <= bounds.length (last segment index)`.
    *
    * @return value assigned to the segment with index `ind`.
    */
  protected def getSegmentValue(ind: Int): W

  /** @return index of segment containing `bound`. */
  protected def searchSegmentFromBegin(bound: Bound[E]): Int

  /**
    * Preconditions:
    *
    * 1. `0 <= ind <= bounds.length (last segment index)`.
    *
    * @return index of segment containing `bound`. Search may be optimized depending on the current index `ind`.
    */
  protected def searchSegmentFromIndex(ind: Int, bound: Bound[E]): Int

  /**
   * @return value of first segment.
   */
  @inline
  protected final def getFirstSegmentValue: W = getSegmentValue(0)

  /**
   * @return value of penultimate segment.
   */
  @inline
  protected final def getPenultimateSegmentValue: W = getSegmentValue(lastBoundIndex)

  /**
   * @return value of last segment.
   */
  @inline
  protected final def getLastSegmentValue: W = getSegmentValue(lastSegmentIndex)

  /**
   * Preconditions:
   *
   * 1. `0 <= ind <= bounds.length (last segment index)`.
   *
   * Returns `true` if segment with given index is considered to be included in set.
   * Segment inclusion is always alternates, therefore it's computed from index and `complementary` flag.
   *
   */
  @inline
  protected final def isIncludedInSet(ind: Int): Boolean = complementary ^ ((ind & 0x00000001) == 0x00000001)

  /**
    * Preconditions:
    *
    * 1. `1 <= ind <= bounds.length (last segment index)`.
    *
    *  @return lower bound of the segment with index `ind`. First segment has no lower bound.
    */
  @inline
  protected final def getLowerBound(ind: Int): Bound.Lower[E] = bounds(ind - 1).flipUpper

  /**
    * Preconditions:
    *
    * 1. `0 <= ind < bounds.length (last segment index)`.
    *
    * @return upper bound of the segment with index `ind`. Last segment has no upper bound.
    */
  @inline
  protected final def getUpperBound(ind: Int): Bound.Upper[E] = bounds(ind)

  @inline
  protected final def lastBoundIndex: Int = bounds.length - 1

  @inline
  protected final def lastSegmentIndex: Int = bounds.length

  /**
    * Preconditions:
    *
    * 1. `0 <= ind <= bounds.length (last segment index)`.
    *
    * @return segment with index `ind`.
    */
  protected final def makeSegment(ind: Int): IndexedSegmentBase with GenSegment =
    if (ind <= 0)                     IndexedInitialSegment()
    else if (ind >= lastSegmentIndex) IndexedTerminalSegment()
    else                              IndexedInnerSegment(ind)

  /**
    * Preconditions:
   *
    * 1. `1 <= ind <= bounds.length (last segment index)`.
    *
    * @return segment (which has previous segment) with index `ind`.
    */
  protected final def makeSegmentWithPrev(ind: Int): IndexedSegmentWithPrev =
    if (ind >= lastSegmentIndex) IndexedTerminalSegment() else IndexedInnerSegment(ind)

  /**
    * Preconditions:
    *
    * 1. `0 <= ind < bounds.length (last segment index)`.
    *
    * @return segment (which has next segment) with index `ind`.
    */
  protected final def makeSegmentWithNext(ind: Int): IndexedSegmentWithNext =
    if (ind <= 0) IndexedInitialSegment() else IndexedInnerSegment(ind)

  /**
    * Base trait for indexed sequence segments. It has either previous or next segment.
    *
    * Preconditions:
    *
    * 1. `0 <= ind <= bounds.length (last segment index)`.
    */
  protected sealed trait IndexedSegmentBase extends SegmentLike[E, D, W] {

    val ind: Int

    override def sequence: SegmentSeq[E, D, W] = seq
    
    override def domainOps: DomainOps[E, D] = seq.domainOps

    override def value: W = getSegmentValue(ind)

    override def isIncluded: Boolean = isIncludedInSet(ind)

    override def moveToFirst: FirstSegment = IndexedInitialSegment()

    override def moveToLast: LastSegment = IndexedTerminalSegment()

    override def moveTo(bound: Bound[E]): GenSegment = makeSegment(searchSegmentFromIndex(ind, bound))
  }

  /**
    * Segment which has next segment.
    *
    * Preconditions:
    *
    * 1. `0 <= ind < bounds.length (last segment index)`.
    */
  protected sealed trait IndexedSegmentWithNext extends IndexedSegmentBase with SegmentWithNext {

    override def upperBound: Bound.Upper[E] = getUpperBound(ind)

    override def moveNext: SegmentWithPrev = makeSegmentWithPrev(ind + 1)
  }

  /**
    * Segment which has previous segment.
    *
    * Preconditions:
    *
    * 1. `1 <= ind <= bounds.length (last segment index)`.
    */
  protected sealed trait IndexedSegmentWithPrev extends IndexedSegmentBase with SegmentWithPrev {

    override def lowerBound: Bound.Lower[E] = getLowerBound(ind)

    override def movePrev: SegmentWithNext = makeSegmentWithNext(ind - 1)
  }

  /** Initial segment of sequence. */
  protected sealed case class IndexedInitialSegment() extends IndexedSegmentWithNext with InitialSegment {

    override val ind: Int = 0
  }

  /** Terminal segment of sequence. */
  protected sealed case class IndexedTerminalSegment() extends IndexedSegmentWithPrev with TerminalSegment {

    override val ind: Int = lastSegmentIndex
  }

  /**
    * Inner segment of sequence. It has both previous and next segments.
    *
    * Preconditions:
    *
    * 1. `1 <= ind < bounds.length (last segment index)`.
    */
  protected sealed case class IndexedInnerSegment(
    override val ind: Int
  ) extends IndexedSegmentWithPrev with IndexedSegmentWithNext with InnerSegment
}