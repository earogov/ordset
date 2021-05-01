package ordset.core

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps

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
abstract class AbstractIndexedSegmentSeq[E, D <: Domain[E],  W] 
  extends AbstractSegmentSeq[E, D, W, AbstractIndexedSegmentSeq.IndexedSegmentBase[E, D, W]] { 
  seq =>

  import AbstractIndexedSegmentSeq._
  
  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = false

  final override def isUniversal: Boolean = false

  final override def isUniform: Boolean = false

  final override def contains(bound: Bound[E]): Boolean = isIncludedInSet(searchSegmentFromBegin(bound))

  final override def contains(element: E): Boolean = super.contains(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = bounds

  final override def firstSegment: IndexedInitialSegment[E, D, W] = IndexedInitialSegment(this)

  final override def lastSegment: IndexedTerminalSegment[E, D, W] = IndexedTerminalSegment(this)

  final override def getSegment(bound: Bound[E]): IndexedSegment[E, D, W] =
    makeSegment(searchSegmentFromBegin(bound))

  final override def getSegment(element: E): IndexedSegment[E, D, W] =
    getSegment(Bound.Upper.inclusive(element))

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): IndexedSegmentSeq[E, D, W] = {
    val ind = searchSegmentFromBegin(bound)
    if (ind == 0) this
    else if (ind == lastSegmentIndex) consUniform(getSegmentValue(ind))
    else consAbove(ind)
  }

  final override def takenBelow(bound: Bound[E]): IndexedSegmentSeq[E, D, W] = {
    val ind = searchSegmentFromBegin(bound)
    if (ind == 0) consUniform(getSegmentValue(ind))
    else if (ind == lastSegmentIndex) this
    else consBelow(ind - 1)
  }

  final override def sliced(bound: Bound[E]): (IndexedSegmentSeq[E, D, W], IndexedSegmentSeq[E, D, W]) = {
    val ind = searchSegmentFromBegin(bound)
    if (ind == 0) (consUniform(getSegmentValue(ind)), this)
    else if (ind == lastSegmentIndex) (this, consUniform(getSegmentValue(ind)))
    else (consBelow(ind - 1), consAbove(ind))
  }
  
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
   * Creates uniform segment sequence (empty or universal).
   *
   * Note current class not supports empty and universal sets so other implementations should be used.
   */
  protected def consUniform(value: W): UniformSegmentSeq[E, D, W]

  /**
   * Preconditions:
   *
   * 1. `0 <= ind <= bounds.length - 1` (last bound index)
   *
   * Creates segment sequence from current keeping only upper bounds with index `>=` `ind`.
   */
  protected def consAbove(ind: Int): AbstractIndexedSegmentSeq[E, D, W]

  /**
   * Preconditions:
   *
   * 1. `0 <= ind <= bounds.length - 1` (last bound index)
   *
   * Creates segment sequence from current keeping only upper bounds with index `<=` `ind`.
   */
  protected def consBelow(ind: Int): AbstractIndexedSegmentSeq[E, D, W]
  
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
  protected final def makeSegment(ind: Int): IndexedSegment[E, D, W] =
    if (ind <= 0)                     IndexedInitialSegment(this)
    else if (ind >= lastSegmentIndex) IndexedTerminalSegment(this)
    else                              IndexedInnerSegment(this, ind)

  /**
    * Preconditions:
   *
    * 1. `1 <= ind <= bounds.length (last segment index)`.
    *
    * @return segment (which has previous segment) with index `ind`.
    */
  protected final def makeSegmentWithPrev(ind: Int): IndexedSegmentWithPrev[E, D, W] =
    if (ind >= lastSegmentIndex) IndexedTerminalSegment(this) else IndexedInnerSegment(this, ind)

  /**
    * Preconditions:
    *
    * 1. `0 <= ind < bounds.length (last segment index)`.
    *
    * @return segment (which has next segment) with index `ind`.
    */
  protected final def makeSegmentWithNext(ind: Int): IndexedSegmentWithNext[E, D, W] =
    if (ind <= 0) IndexedInitialSegment(this) else IndexedInnerSegment(this, ind)
}

object AbstractIndexedSegmentSeq {

  type IndexedSegment[E, D <: Domain[E], W] = 
    SegmentT[E, D, W, IndexedSegmentBase[E, D, W]] with IndexedSegmentBase[E, D, W]

  /**
   * Base trait for indexed sequence segments. It has either previous or next segment.
   *
   * Preconditions:
   *
   * 1. `0 <= ind <= bounds.length (last segment index)`.
   */
  sealed trait IndexedSegmentBase[E, D <: Domain[E], W]
    extends SegmentLikeT[E, D, W, IndexedSegmentBase[E, D, W]] {

    // Inspection --------------------------------------------------------------- //
    val index: Int

    override val sequence: AbstractIndexedSegmentSeq[E, D, W]

    override def value: W = sequence.getSegmentValue(index)

    override def isIncluded: Boolean = sequence.isIncludedInSet(index)

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: IndexedInitialSegment[E, D, W] = IndexedInitialSegment(sequence)

    override def moveToLast: IndexedTerminalSegment[E, D, W] = IndexedTerminalSegment(sequence)

    override def moveTo(bound: Bound[E]): IndexedSegment[E, D, W] =
      sequence.makeSegment(sequence.searchSegmentFromIndex(index, bound))

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: IndexedSegmentSeq[E, D, W]

    override def takenBelow: IndexedSegmentSeq[E, D, W]

    override def sliced: (IndexedSegmentSeq[E, D, W], IndexedSegmentSeq[E, D, W])
  }

  /**
   * Segment which has next segment.
   *
   * Preconditions:
   *
   * 1. `0 <= ind < bounds.length (last segment index)`.
   */
  sealed trait IndexedSegmentWithNext[E, D <: Domain[E], W]
    extends SegmentT.WithNext[E, D, W, IndexedSegmentBase[E, D, W]]
      with IndexedSegmentBase[E, D, W] {

    // Inspection --------------------------------------------------------------- //
    override def upperBound: Bound.Upper[E] = sequence.getUpperBound(index)

    // Navigation --------------------------------------------------------------- //
    override def moveNext: IndexedSegmentWithPrev[E, D, W] = sequence.makeSegmentWithPrev(index + 1)

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: AbstractIndexedSegmentSeq[E, D, W]

    override def sliced: (IndexedSegmentSeq[E, D, W], AbstractIndexedSegmentSeq[E, D, W])
  }

  /**
   * Segment which has previous segment.
   *
   * Preconditions:
   *
   * 1. `1 <= ind <= bounds.length (last segment index)`.
   */
  sealed trait IndexedSegmentWithPrev[E, D <: Domain[E], W]
    extends SegmentT.WithPrev[E, D, W, IndexedSegmentBase[E, D, W]]
      with IndexedSegmentBase[E, D, W] {

    // Inspection --------------------------------------------------------------- //
    override def lowerBound: Bound.Lower[E] = sequence.getLowerBound(index)

    // Navigation --------------------------------------------------------------- //
    override def movePrev: IndexedSegmentWithNext[E, D, W] = sequence.makeSegmentWithNext(index - 1)

    // Transformation ----------------------------------------------------------- //
    override def takenBelow: AbstractIndexedSegmentSeq[E, D, W]

    override def sliced: (AbstractIndexedSegmentSeq[E, D, W], IndexedSegmentSeq[E, D, W])
  }

  /** Initial segment of sequence. */
  final case class IndexedInitialSegment[E, D <: Domain[E], W](
    override val sequence: AbstractIndexedSegmentSeq[E, D, W]
  ) extends SegmentT.Initial[E, D, W, IndexedSegmentBase[E, D, W]]
    with IndexedSegmentWithNext[E, D, W] {

    // Inspection --------------------------------------------------------------- //
    override val index: Int = 0

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: IndexedInitialSegment[E, D, W] = this

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: AbstractIndexedSegmentSeq[E, D, W] = sequence

    override def takenBelow: AbstractUniformSegmentSeq[E, D, W] = sequence.consUniform(value)

    override def sliced: (AbstractUniformSegmentSeq[E, D, W], AbstractIndexedSegmentSeq[E, D, W]) =
      (takenBelow, takenAbove)

    // Protected section -------------------------------------------------------- //
    protected override def self: IndexedInitialSegment[E, D, W] = this
  }

  /** Terminal segment of sequence. */
  final case class IndexedTerminalSegment[E, D <: Domain[E], W](
    override val sequence: AbstractIndexedSegmentSeq[E, D, W]
  ) extends SegmentT.Terminal[E, D, W, IndexedSegmentBase[E, D, W]]
    with IndexedSegmentWithPrev[E, D, W] {

    // Inspection --------------------------------------------------------------- //
    override val index: Int = sequence.lastSegmentIndex

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: IndexedTerminalSegment[E, D, W] = this
    
    // Transformation ----------------------------------------------------------- //
    override def takenAbove: AbstractUniformSegmentSeq[E, D, W] = sequence.consUniform(value)

    override def takenBelow: AbstractIndexedSegmentSeq[E, D, W] = sequence

    override def sliced: (AbstractIndexedSegmentSeq[E, D, W], AbstractUniformSegmentSeq[E, D, W]) =
      (takenBelow, takenAbove)

    // Protected section -------------------------------------------------------- //
    protected override def self: IndexedTerminalSegment[E, D, W] = this
  }

  /**
   * Inner segment of sequence. It has both previous and next segments.
   *
   * Preconditions:
   *
   * 1. `1 <= ind < bounds.length (last segment index)`.
   */
  final case class IndexedInnerSegment[E, D <: Domain[E], W](
    override val sequence: AbstractIndexedSegmentSeq[E, D, W],
    override val index: Int
  ) extends SegmentT.Inner[E, D, W, IndexedSegmentBase[E, D, W]]
    with IndexedSegmentWithPrev[E, D, W]
    with IndexedSegmentWithNext[E, D, W] {

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: AbstractIndexedSegmentSeq[E, D, W] = sequence.consAbove(index)

    override def takenBelow: AbstractIndexedSegmentSeq[E, D, W] = sequence.consBelow(index - 1)

    override def sliced: (AbstractIndexedSegmentSeq[E, D, W], AbstractIndexedSegmentSeq[E, D, W]) =
      (takenBelow, takenAbove)

    // Protected section -------------------------------------------------------- //
    protected override def self: IndexedInnerSegment[E, D, W] = this
  }
}