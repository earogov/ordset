package ordset.core

import ordset.core.AbstractIndexedSegmentSeq._
import ordset.core.SegmentT.{Initial, Inner, Terminal}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps

import scala.collection.Seq

/**
 * For common description of segment sequence see [[SegmentSeq]].
 *
 * <u>Class is not intended to model empty and universal sets.</u>
 * For such cases implementation based on [[UniformSegmentSeq]] can be used.
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
 * <tr>`bounds` collection MUST be non-empty.                   </tr>
 * <tr>`bounds` collection SHOULD provide fast access by index. </tr>
 */
abstract class AbstractIndexedSegmentSeq[E, D <: Domain[E],  V] 
  extends AbstractSegmentSeq[E, D, V, IndexedSegmentBase[E, D, V]] {
  
  // Inspection --------------------------------------------------------------- //
  /**
   * Collection of segments upper bounds.
   *
   * It MUST be non-empty.
   */
  val bounds: Seq[Bound.Upper[E]]

  /** When false, first segment doesn't belong to set and vice versa.*/
  val complementary: Boolean

  final override def isEmpty: Boolean = false

  final override def isUniversal: Boolean = false

  final override def isUniform: Boolean = false

  final override def includesBound(bound: Bound[E]): Boolean = isIndexIncluded(searchSegmentFromBegin(bound))

  final override def includesExtended(bound: ExtendedBound[E]): Boolean =
    bound match {
      case b: Bound[E] => includesBound(b)
      case ExtendedBound.BelowAll => isIndexIncluded(0)
      case ExtendedBound.AboveAll => isIndexIncluded(lastSegmentIndex)
    }
  
  final override def includesElement(element: E): Boolean = super.includesElement(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = bounds

  final override def firstSegment: IndexedInitialSegment[E, D, V] = IndexedInitialSegment(this)

  /**
   * @return second segment of sequence.
   */
  final def secondSegment: IndexedSegmentWithPrev[E, D, V] = makeSegmentWithPrev(1)
  
  final override def lastSegment: IndexedTerminalSegment[E, D, V] = IndexedTerminalSegment(this)

  /**
   * @return penultimate segment of sequence.
   */
  final def penultimateSegment: IndexedSegmentWithNext[E, D, V] = makeSegmentWithNext(lastSegmentIndex - 1)
  
  final override def getSegmentForBound(bound: Bound[E]): IndexedSegment[E, D, V] =
    makeSegment(searchSegmentFromBegin(bound))

  final override def getSegmentForExtended(bound: ExtendedBound[E]): IndexedSegment[E, D, V] = 
    super.getSegmentForExtended(bound)
  
  final override def getSegmentForElement(element: E): IndexedSegment[E, D, V] =
    super.getSegmentForElement(element)

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): IndexedSegmentSeq[E, D, V] = {
    val ind = searchSegmentFromBegin(bound)
    if (ind == 0) this
    else if (ind == lastSegmentIndex) consUniform(getSegmentValue(ind))
    else consAbove(ind)
  }

  final override def takenBelow(bound: Bound[E]): IndexedSegmentSeq[E, D, V] = {
    val ind = searchSegmentFromBegin(bound)
    if (ind == 0) consUniform(getSegmentValue(ind))
    else if (ind == lastSegmentIndex) this
    else consBelow(ind - 1)
  }

  final override def sliced(bound: Bound[E]): (IndexedSegmentSeq[E, D, V], IndexedSegmentSeq[E, D, V]) = {
    val ind = searchSegmentFromBegin(bound)
    if (ind == 0) (consUniform(getSegmentValue(ind)), this)
    else if (ind == lastSegmentIndex) (this, consUniform(getSegmentValue(ind)))
    else (consBelow(ind - 1), consAbove(ind))
  }

  // Transformation ----------------------------------------------------------- //
  final override def prepended(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = {
    val segment = secondSegment
    prependedInternal(segment.lowerBound, segment, other)
  }

  final override def prepended(bound: Bound[E], other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
    prependedInternal(bound, getSegmentForBound(bound.provideLower), other)

  final override def appended(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = {
    val segment = penultimateSegment
    appendedInternal(segment.upperBound, segment, other)
  }
  
  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
    appendedInternal(bound, getSegmentForBound(bound.provideUpper), other)

  // Protected section -------------------------------------------------------- //
  /**
   * Validate state after initialization.
   */
  @throws[SegmentSeqException]("if bounds collection is empty")
  protected def validate(): Unit = {
    if (bounds.isEmpty) throw SegmentSeqException("Bounds collection must be non-empty.")
  }

  /**
    * Preconditions:
    *
    * 1. `0 <= ind <= bounds.length (last segment index)`.
    *
    * @return value assigned to the segment with index `ind`.
    */
  protected def getSegmentValue(ind: Int): V

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
  protected def consUniform(value: V): UniformSegmentSeq[E, D, V]

  /**
   * Preconditions:
   *
   * 1. `0 <= ind <= bounds.length - 1` (last bound index)
   *
   * Creates segment sequence from current keeping only upper bounds with index `>=` `ind`.
   */
  protected def consAbove(ind: Int): NonuniformIndexedSegmentSeq[E, D, V]

  /**
   * Preconditions:
   *
   * 1. `0 <= ind <= bounds.length - 1` (last bound index)
   *
   * Creates segment sequence from current keeping only upper bounds with index `<=` `ind`.
   */
  protected def consBelow(ind: Int): NonuniformIndexedSegmentSeq[E, D, V]

  /**
   * Same as [[SegmentSeqT.prepended]] but with additional argument `originalBoundSegment` such that:
   * {{{
   *   originalBoundSegment = this.getSegment(bound.provideLower)    (1)
   * }}}
   * This allows to avoid repeated search of segment if it's already known before method call.
   *
   * Note if provided segment other then one defined by condition 1, the behaviour of method is undefined.
   */
  protected def prependedInternal(
    bound: Bound[E],
    originalBoundSegment: IndexedSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): IndexedSegmentSeq[E, D, V]

  /**
   * Same as [[SegmentSeqT.appended]] but with additional argument `originalBoundSegment` such that:
   * {{{
   *   originalBoundSegment = this.getSegment(bound.provideUpper)    (1)
   * }}}
   * This allows to avoid repeated search of segment if it's already known before method call.
   *
   * Note if provided segment other then one defined by condition 1, the behaviour of method is undefined.
   */
  protected def appendedInternal(
    bound: Bound[E],
    originalBoundSegment: IndexedSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): IndexedSegmentSeq[E, D, V]

  /**
   * Preconditions:
   *
   * 1. `0 <= ind <= bounds.length (last segment index)`.
   *
   * Returns `true` if segment with given index is considered to be included in set.
   * Segment inclusion is always alternates, therefore it's computed from index and `complementary` flag.
   *
   */
  protected def isIndexIncluded(ind: Int): Boolean

  /**
   * @return value of first segment.
   */
  @inline
  protected final def getFirstSegmentValue: V = getSegmentValue(0)

  /**
   * @return value of last segment.
   */
  @inline
  protected final def getLastSegmentValue: V = getSegmentValue(lastSegmentIndex)

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
  protected final def makeSegment(ind: Int): IndexedSegment[E, D, V] =
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
  protected final def makeSegmentWithPrev(ind: Int): IndexedSegmentWithPrev[E, D, V] =
    if (ind >= lastSegmentIndex) IndexedTerminalSegment(this) else IndexedInnerSegment(this, ind)

  /**
    * Preconditions:
    *
    * 1. `0 <= ind < bounds.length (last segment index)`.
    *
    * @return segment (which has next segment) with index `ind`.
    */
  protected final def makeSegmentWithNext(ind: Int): IndexedSegmentWithNext[E, D, V] =
    if (ind <= 0) IndexedInitialSegment(this) else IndexedInnerSegment(this, ind)
}

object AbstractIndexedSegmentSeq {

  type IndexedSegment[E, D <: Domain[E], V] =
    SegmentT[E, D, V, IndexedSegmentBase[E, D, V]] with IndexedSegmentBase[E, D, V]

  /**
   * Base trait for indexed sequence segments. It has either previous or next segment.
   *
   * Preconditions:
   *
   * 1. `0 <= ind <= bounds.length (last segment index)`.
   */
  sealed trait IndexedSegmentBase[E, D <: Domain[E], V]
    extends SegmentLikeT[E, D, V, IndexedSegmentBase[E, D, V]] {

    // Inspection --------------------------------------------------------------- //
    val index: Int

    override val sequence: NonuniformIndexedSegmentSeq[E, D, V]

    override def value: V = sequence.getSegmentValue(index)

    override def isIncluded: Boolean = sequence.isIndexIncluded(index)

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: IndexedInitialSegment[E, D, V] = IndexedInitialSegment(sequence)

    override def moveToLast: IndexedTerminalSegment[E, D, V] = IndexedTerminalSegment(sequence)

    override def moveToBound(bound: Bound[E]): IndexedSegment[E, D, V] =
      sequence.makeSegment(sequence.searchSegmentFromIndex(index, bound))

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: IndexedSegmentSeq[E, D, V]

    override def takenBelow: IndexedSegmentSeq[E, D, V]

    override def sliced: (IndexedSegmentSeq[E, D, V], IndexedSegmentSeq[E, D, V])

    override def prepended(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = {
      // Default implementation for first segment. Must be overridden if segment has previous segment.
      sequence
    }
    
    override def appended(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = {
      // Default implementation for last segment. Must be overridden if segment has next segment.
      sequence
    }

    override def patched(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = {
      // We need to override method here to provide more concrete type.
      // But we can't make method abstract due to conflict with implementation in parent trait.
      // So we just throw exception here and real implemented is provided subclasses.
      // Trait is sealed so this is unreachable case.
      throw new AssertionError("Implementation is provided in subclasses of sealed trait.")
    }
  }

  object IndexedSegmentBase {

    trait TruncationBase[E, D <: Domain[E], V] {
      self: SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], IndexedSegment[E, D, V]] =>

      override def prepended(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
        segment.sequence.prependedInternal(bound, getPrependedBoundSegment, other)

      override def appended(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
        segment.sequence.appendedInternal(bound, getAppendedBoundSegment, other)
    }
  }

  /**
   * Segment which has next segment.
   *
   * Preconditions:
   *
   * 1. `0 <= ind < bounds.length (last segment index)`.
   */
  sealed trait IndexedSegmentWithNext[E, D <: Domain[E], V]
    extends SegmentT.WithNext[E, D, V, IndexedSegmentBase[E, D, V]]
      with IndexedSegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: IndexedSegmentWithNext[E, D, V]

    override def upperBound: Bound.Upper[E] = sequence.getUpperBound(index)

    // Navigation --------------------------------------------------------------- //
    override def moveNext: IndexedSegmentWithPrev[E, D, V] = sequence.makeSegmentWithPrev(index + 1)

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: NonuniformIndexedSegmentSeq[E, D, V]

    override def sliced: (IndexedSegmentSeq[E, D, V], NonuniformIndexedSegmentSeq[E, D, V])

    override def appended(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
      sequence.appendedInternal(upperBound, this, other)
  }

  /**
   * Segment which has previous segment.
   *
   * Preconditions:
   *
   * 1. `1 <= ind <= bounds.length (last segment index)`.
   */
  sealed trait IndexedSegmentWithPrev[E, D <: Domain[E], V]
    extends SegmentT.WithPrev[E, D, V, IndexedSegmentBase[E, D, V]]
      with IndexedSegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: IndexedSegmentWithPrev[E, D, V]
    
    override def lowerBound: Bound.Lower[E] = sequence.getLowerBound(index)

    // Navigation --------------------------------------------------------------- //
    override def movePrev: IndexedSegmentWithNext[E, D, V] = sequence.makeSegmentWithNext(index - 1)

    // Transformation ----------------------------------------------------------- //
    override def takenBelow: NonuniformIndexedSegmentSeq[E, D, V]

    override def sliced: (NonuniformIndexedSegmentSeq[E, D, V], IndexedSegmentSeq[E, D, V])

    override def prepended(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
      sequence.prependedInternal(lowerBound, this, other)
  }

  /** Initial segment of sequence. */
  final case class IndexedInitialSegment[E, D <: Domain[E], V](
    override val sequence: NonuniformIndexedSegmentSeq[E, D, V]
  ) extends SegmentT.Initial[E, D, V, IndexedSegmentBase[E, D, V]]
    with IndexedSegmentWithNext[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override val index: Int = 0

    override def self: IndexedInitialSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: IndexedInitialSegment[E, D, V] = this

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: NonuniformIndexedSegmentSeq[E, D, V] = sequence

    override def takenBelow: UniformSegmentSeq[E, D, V] = sequence.consUniform(value)

    override def sliced: (UniformSegmentSeq[E, D, V], NonuniformIndexedSegmentSeq[E, D, V]) =
      (takenBelow, takenAbove)

    override def patched(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = moveNext.prepended(other)

    override def truncation(bound: Bound[E]): IndexedInitialSegment.Truncation[E, D, V, this.type] =
      new IndexedInitialSegment.Truncation(this, bound)
  }

  object IndexedInitialSegment {

    final class Truncation[E, D <: Domain[E], V, +Seg <: IndexedInitialSegment[E, D, V]](
      override val segment: Seg,
      inputBound: Bound[E],
    ) extends SegmentT.Initial.Truncation[E, D, V, IndexedSegmentBase[E, D, V], Seg](
      segment,
      inputBound,
    ) with IndexedSegmentBase.TruncationBase[E, D, V]
  }

  /** Terminal segment of sequence. */
  final case class IndexedTerminalSegment[E, D <: Domain[E], V](
    override val sequence: NonuniformIndexedSegmentSeq[E, D, V]
  ) extends SegmentT.Terminal[E, D, V, IndexedSegmentBase[E, D, V]]
    with IndexedSegmentWithPrev[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override val index: Int = sequence.lastSegmentIndex

    override def self: IndexedTerminalSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: IndexedTerminalSegment[E, D, V] = this
    
    // Transformation ----------------------------------------------------------- //
    override def takenAbove: UniformSegmentSeq[E, D, V] = sequence.consUniform(value)

    override def takenBelow: NonuniformIndexedSegmentSeq[E, D, V] = sequence

    override def sliced: (NonuniformIndexedSegmentSeq[E, D, V], UniformSegmentSeq[E, D, V]) =
      (takenBelow, takenAbove)

    override def patched(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = movePrev.appended(other)

    override def truncation(bound: Bound[E]): IndexedTerminalSegment.Truncation[E, D, V, this.type] =
      new IndexedTerminalSegment.Truncation(this, bound)
  }

  object IndexedTerminalSegment {

    final class Truncation[E, D <: Domain[E], V, +Seg <: IndexedTerminalSegment[E, D, V]](
      override val segment: Seg,
      inputBound: Bound[E],
    ) extends SegmentT.Terminal.Truncation[E, D, V, IndexedSegmentBase[E, D, V], Seg](
      segment,
      inputBound,
    ) with IndexedSegmentBase.TruncationBase[E, D, V]
  }

  /**
   * Inner segment of sequence. It has both previous and next segments.
   *
   * Preconditions:
   *
   * 1. `1 <= ind < bounds.length (last segment index)`.
   */
  final case class IndexedInnerSegment[E, D <: Domain[E], V](
    override val sequence: NonuniformIndexedSegmentSeq[E, D, V],
    override val index: Int
  ) extends SegmentT.Inner[E, D, V, IndexedSegmentBase[E, D, V]]
    with IndexedSegmentWithPrev[E, D, V]
    with IndexedSegmentWithNext[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: IndexedInnerSegment[E, D, V] = this
    
    // Transformation ----------------------------------------------------------- //
    override def takenAbove: NonuniformIndexedSegmentSeq[E, D, V] = sequence.consAbove(index)

    override def takenBelow: NonuniformIndexedSegmentSeq[E, D, V] = sequence.consBelow(index - 1)

    override def sliced: (NonuniformIndexedSegmentSeq[E, D, V], NonuniformIndexedSegmentSeq[E, D, V]) =
      (takenBelow, takenAbove)

    override def patched(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
      moveNext.prepended(movePrev.appended(other))

    override def truncation(bound: Bound[E]): IndexedInnerSegment.Truncation[E, D, V, this.type] =
      new IndexedInnerSegment.Truncation(this, bound)
  }

  object IndexedInnerSegment {

    final class Truncation[E, D <: Domain[E], V, +Seg <: IndexedInnerSegment[E, D, V]](
      override val segment: Seg,
      inputBound: Bound[E],
    ) extends SegmentT.Inner.Truncation[E, D, V, IndexedSegmentBase[E, D, V], Seg](
      segment,
      inputBound,
    ) with IndexedSegmentBase.TruncationBase[E, D, V]
  }
}