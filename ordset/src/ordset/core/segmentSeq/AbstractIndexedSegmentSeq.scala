package ordset.core.segmentSeq

import ordset.core.{Bound, ExtendedBound}
import ordset.core.segmentSeq.AbstractIndexedSegmentSeq.*
import ordset.core.segmentSeq.SegmentT.{Initial, Inner, Terminal}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps

import scala.collection.Seq

/**
 * Implementation of segment sequence (see [[SegmentSeq]]) based on collection with efficient indexed access 
 * (array, etc).
 *
 * Upper bounds of segments are stored in `bounds` collection:
 * {{{
 *
 *   Segment 0       Segment 1       Segment 2   - segment index
 *              0                 1              - bound index
 * -------------|-----------------|------------
 *       A               B              C        - value
 * }}}
 * Upper bound of last segment is not stored.
 * 
 * <u>Class is not intended to model empty and universal sets.</u>
 * For such cases implementation based on [[UniformSegmentSeq]] should be used.
 *
 * Preconditions:
 * <div>`bounds` collection must be non-empty.                   </div>
 * <div>`bounds` collection should provide fast access by index. </div>
 */
abstract class AbstractIndexedSegmentSeq[E, D[X] <: Domain[X],  V] 
  extends AbstractStrictSegmentSeq[E, D, V, IndexedSegmentBase[E, D, V]] {
  
  // Inspection --------------------------------------------------------------- //
  /**
   * Collection of segments upper bounds.
   *
   * It must be non-empty.
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

  final override def extendedUpperBounds: Iterable[ExtendedBound.Upper[E]] = bounds.appended(ExtendedBound.AboveAll)

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

  final override def getValueForBound(bound: Bound[E]): V =
    getSegmentValue(searchSegmentFromBegin(bound))

  final override def getValueForExtended(bound: ExtendedBound[E]): V =
    bound match {
      case bound: Bound[E] => getValueForBound(bound)
      case ExtendedBound.BelowAll => getFirstSegmentValue
      case ExtendedBound.AboveAll => getLastSegmentValue
    }

  final override def getValueForElement(element: E): V = 
    super.getValueForElement(element)
  
  // Transformation ----------------------------------------------------------- //
  final override def takeAboveBound(bound: Bound[E]): IndexedSegmentSeq[E, D, V] = {
    val ind = searchSegmentFromBegin(bound)
    if (ind == 0) this
    else if (ind == lastSegmentIndex) consUniform(getSegmentValue(ind))
    else consAbove(ind)
  }

  final override def takeAboveExtended(bound: ExtendedBound[E]): IndexedSegmentSeq[E, D, V] = 
    bound match {
      case bound: Bound[E] => takeAboveBound(bound)
      case ExtendedBound.BelowAll => this
      case ExtendedBound.AboveAll => consUniform(lastSegment.value)
    }

  final override def takeBelowBound(bound: Bound[E]): IndexedSegmentSeq[E, D, V] = {
    val ind = searchSegmentFromBegin(bound)
    if (ind == 0) consUniform(getSegmentValue(ind))
    else if (ind == lastSegmentIndex) this
    else consBelow(ind - 1)
  }

  final override def takeBelowExtended(bound: ExtendedBound[E]): IndexedSegmentSeq[E, D, V] = 
    bound match {
      case bound: Bound[E] => takeBelowBound(bound)
      case ExtendedBound.BelowAll => consUniform(firstSegment.value)
      case ExtendedBound.AboveAll => this
    }
  
  final override def sliceAtBound(bound: Bound[E]): (IndexedSegmentSeq[E, D, V], IndexedSegmentSeq[E, D, V]) = {
    val ind = searchSegmentFromBegin(bound)
    if (ind == 0) (consUniform(getSegmentValue(ind)), this)
    else if (ind == lastSegmentIndex) (this, consUniform(getSegmentValue(ind)))
    else (consBelow(ind - 1), consAbove(ind))
  }

  final override def sliceAtExtended(
    bound: ExtendedBound[E]
  ): (IndexedSegmentSeq[E, D, V], IndexedSegmentSeq[E, D, V]) = 
    bound match {
      case bound: Bound[E] => sliceAtBound(bound)
      case ExtendedBound.BelowAll => (consUniform(firstSegment.value), this)
      case ExtendedBound.AboveAll => (this, consUniform(lastSegment.value))
    }
  
  final override def prepend(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = {
    val segment = secondSegment
    prependBelowBoundInternal(segment.lower, segment, other)
  }

  final override def prependBelowBound(bound: Bound[E], other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
    prependBelowBoundInternal(bound, getSegmentForBound(bound.provideLower), other)

  final override def prependBelowExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = 
    super.prependBelowExtended(bound, other)
  
  final override def append(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = {
    val segment = penultimateSegment
    appendAboveBoundInternal(segment.upper, segment, other)
  }

  final override def appendAboveBound(bound: Bound[E], other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
    appendAboveBoundInternal(bound, getSegmentForBound(bound.provideUpper), other)

  final override def appendAboveExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = 
    super.appendAboveExtended(bound, other)

  final override def patchLazy(supplierSeq: SupplierSegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    patchLazyDelayedInternal(supplierSeq)
  
  // Protected section -------------------------------------------------------- //
  protected final override type SegmentInternal = IndexedSegment[E, D, V]

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

  protected override def consUniform(value: V): UniformSegmentSeq[E, D, V]

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

  protected def prependBelowBoundInternal(
    bound: Bound[E],
    originalBoundSegment: IndexedSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): IndexedSegmentSeq[E, D, V]

  protected final override def prependBelowExtendedInternal(
    bound: ExtendedBound[E],
    originalBoundSegment: IndexedSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] =
    super.prependBelowExtendedInternal(bound, originalBoundSegment, other)

  protected def appendAboveBoundInternal(
    bound: Bound[E],
    originalBoundSegment: IndexedSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): IndexedSegmentSeq[E, D, V]

  protected final override def appendAboveExtendedInternal(
    bound: ExtendedBound[E],
    originalBoundSegment: IndexedSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] =
    super.appendAboveExtendedInternal(bound, originalBoundSegment, other)

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

  type IndexedSegment[E, D[X] <: Domain[X], V] =
    SegmentT[E, D, V, IndexedSegmentBase[E, D, V]] with IndexedSegmentBase[E, D, V]
  
  type IndexedTruncation[E, D[X] <: Domain[X], V] =
    SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], IndexedSegment[E, D, V]]

  /**
   * Base trait for indexed sequence segments. It has either previous or next segment.
   *
   * Preconditions:
   *
   * 1. `0 <= ind <= bounds.length (last segment index)`.
   */
  sealed trait IndexedSegmentBase[E, D[X] <: Domain[X], V]
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
    override def takeAbove: IndexedSegmentSeq[E, D, V]

    override def takeBelow: IndexedSegmentSeq[E, D, V]

    override def slice: (IndexedSegmentSeq[E, D, V], IndexedSegmentSeq[E, D, V])

    override def prepend(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = {
      // Default implementation for first segment. Must be overridden if segment has previous segment.
      sequence
    }

    override def append(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = {
      // Default implementation for last segment. Must be overridden if segment has next segment.
      sequence
    }

    override def patch(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = {
      // We need to override method here to provide more concrete type.
      // But we can't make method abstract due to conflict with implementation in parent trait.
      // So we just throw exception here and real implemented is provided in subclasses.
      // Trait is sealed so this is unreachable case.
      throw new AssertionError("Implementation is provided in subclasses of sealed trait.")
    }
  }

  object IndexedSegmentBase {

    trait TruncationBase[E, D[X] <: Domain[X], V] {
      self: SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], IndexedSegment[E, D, V]] =>

      override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
        segment.sequence.prependBelowExtendedInternal(bound, getSegmentForPrepending, other)

      override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
        segment.sequence.appendAboveExtendedInternal(bound, getSegmentForAppending, other)
    }
  }

  /**
   * Segment which has next segment.
   *
   * Preconditions:
   *
   * 1. `0 <= ind < bounds.length (last segment index)`.
   */
  sealed trait IndexedSegmentWithNext[E, D[X] <: Domain[X], V]
    extends SegmentT.WithNext[E, D, V, IndexedSegmentBase[E, D, V]]
      with IndexedSegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: IndexedSegmentWithNext[E, D, V]

    override def upper: Bound.Upper[E] = sequence.getUpperBound(index)

    // Navigation --------------------------------------------------------------- //
    override def moveNext: IndexedSegmentWithPrev[E, D, V] = sequence.makeSegmentWithPrev(index + 1)

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: NonuniformIndexedSegmentSeq[E, D, V]

    override def slice: (IndexedSegmentSeq[E, D, V], NonuniformIndexedSegmentSeq[E, D, V])

    override def append(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
      sequence.appendAboveBoundInternal(upper, this, other)
  }

  /**
   * Segment which has previous segment.
   *
   * Preconditions:
   *
   * 1. `1 <= ind <= bounds.length (last segment index)`.
   */
  sealed trait IndexedSegmentWithPrev[E, D[X] <: Domain[X], V]
    extends SegmentT.WithPrev[E, D, V, IndexedSegmentBase[E, D, V]]
      with IndexedSegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: IndexedSegmentWithPrev[E, D, V]

    override def lower: Bound.Lower[E] = sequence.getLowerBound(index)

    // Navigation --------------------------------------------------------------- //
    override def movePrev: IndexedSegmentWithNext[E, D, V] = sequence.makeSegmentWithNext(index - 1)

    // Transformation ----------------------------------------------------------- //
    override def takeBelow: NonuniformIndexedSegmentSeq[E, D, V]

    override def slice: (NonuniformIndexedSegmentSeq[E, D, V], IndexedSegmentSeq[E, D, V])

    override def prepend(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
      sequence.prependBelowBoundInternal(lower, this, other)
  }

  /** Initial segment of sequence. */
  final case class IndexedInitialSegment[E, D[X] <: Domain[X], V](
    override val sequence: NonuniformIndexedSegmentSeq[E, D, V]
  ) extends SegmentT.Initial[E, D, V, IndexedSegmentBase[E, D, V]]
    with IndexedSegmentWithNext[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override val index: Int = 0

    override def self: IndexedInitialSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: IndexedInitialSegment[E, D, V] = this

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: NonuniformIndexedSegmentSeq[E, D, V] = sequence

    override def takeBelow: UniformSegmentSeq[E, D, V] = sequence.consUniform(value)

    override def slice: (UniformSegmentSeq[E, D, V], NonuniformIndexedSegmentSeq[E, D, V]) =
      (takeBelow, takeAbove)

    override def patch(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = moveNext.prepend(other)

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], this.type] =
      new IndexedInitialSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], this.type] =
      SegmentTruncationT.upperTruncation(this)
  }

  object IndexedInitialSegment {

    final class Truncation[E, D[X] <: Domain[X], V, +Seg <: IndexedInitialSegment[E, D, V]](
      override val segment: Seg,
      inputBound: ExtendedBound[E],
    ) extends SegmentT.Initial.Truncation[E, D, V, IndexedSegmentBase[E, D, V], Seg](
      segment,
      inputBound,
    ) with IndexedSegmentBase.TruncationBase[E, D, V]
  }

  /** Terminal segment of sequence. */
  final case class IndexedTerminalSegment[E, D[X] <: Domain[X], V](
    override val sequence: NonuniformIndexedSegmentSeq[E, D, V]
  ) extends SegmentT.Terminal[E, D, V, IndexedSegmentBase[E, D, V]]
    with IndexedSegmentWithPrev[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override val index: Int = sequence.lastSegmentIndex

    override def self: IndexedTerminalSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: IndexedTerminalSegment[E, D, V] = this
    
    // Transformation ----------------------------------------------------------- //
    override def takeAbove: UniformSegmentSeq[E, D, V] = sequence.consUniform(value)

    override def takeBelow: NonuniformIndexedSegmentSeq[E, D, V] = sequence

    override def slice: (NonuniformIndexedSegmentSeq[E, D, V], UniformSegmentSeq[E, D, V]) =
      (takeBelow, takeAbove)

    override def patch(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] = movePrev.append(other)

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], this.type] =
      new IndexedTerminalSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], this.type] =
      SegmentTruncationT.upperTruncation(this)
  }

  object IndexedTerminalSegment {

    final class Truncation[E, D[X] <: Domain[X], V, +Seg <: IndexedTerminalSegment[E, D, V]](
      override val segment: Seg,
      inputBound: ExtendedBound[E],
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
  final case class IndexedInnerSegment[E, D[X] <: Domain[X], V](
    override val sequence: NonuniformIndexedSegmentSeq[E, D, V],
    override val index: Int
  ) extends SegmentT.Inner[E, D, V, IndexedSegmentBase[E, D, V]]
    with IndexedSegmentWithPrev[E, D, V]
    with IndexedSegmentWithNext[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def self: IndexedInnerSegment[E, D, V] = this
    
    // Transformation ----------------------------------------------------------- //
    override def takeAbove: NonuniformIndexedSegmentSeq[E, D, V] = sequence.consAbove(index)

    override def takeBelow: NonuniformIndexedSegmentSeq[E, D, V] = sequence.consBelow(index - 1)

    override def slice: (NonuniformIndexedSegmentSeq[E, D, V], NonuniformIndexedSegmentSeq[E, D, V]) =
      (takeBelow, takeAbove)

    override def patch(other: SegmentSeq[E, D, V]): IndexedSegmentSeq[E, D, V] =
      moveNext.prepend(movePrev.append(other))

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], this.type] =
      new IndexedInnerSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, IndexedSegmentBase[E, D, V], this.type] =
      SegmentTruncationT.upperTruncation(this)
  }

  object IndexedInnerSegment {

    final class Truncation[E, D[X] <: Domain[X], V, +Seg <: IndexedInnerSegment[E, D, V]](
      override val segment: Seg,
      inputBound: ExtendedBound[E],
    ) extends SegmentT.Inner.Truncation[E, D, V, IndexedSegmentBase[E, D, V], Seg](
      segment,
      inputBound,
    ) with IndexedSegmentBase.TruncationBase[E, D, V]
  }
}