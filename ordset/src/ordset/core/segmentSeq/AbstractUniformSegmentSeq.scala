package ordset.core.segmentSeq

import ordset.core.{Bound, ExtendedBound}
import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps}
import AbstractUniformSegmentSeq.*

abstract class AbstractUniformSegmentSeq[E, D[X] <: Domain[X],  V]
  extends AbstractStrictSegmentSeq[E, D, V, UniformSingleSegment[E, D, V]] {

  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = !isValueIncluded(value)

  final override def isUniversal: Boolean = isValueIncluded(value)

  final override def isUniform: Boolean = true

  final override def includesBound(bound: Bound[E]): Boolean = isUniversal

  final override def includesExtended(bound: ExtendedBound[E]): Boolean = isUniversal

  final override def includesElement(element: E): Boolean = isUniversal

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = Iterable.empty

  final override def extendedUpperBounds: Iterable[ExtendedBound.Upper[E]] = Iterable.single(ExtendedBound.AboveAll)

  final override def firstSegment: UniformSingleSegment[E, D, V] = segment

  final override def lastSegment: UniformSingleSegment[E, D, V] = segment

  final override def getSegmentForBound(bound: Bound[E]): UniformSingleSegment[E, D, V] = segment

  final override def getSegmentForExtended(bound: ExtendedBound[E]): UniformSingleSegment[E, D, V] = segment
  
  final override def getSegmentForElement(element: E): UniformSingleSegment[E, D, V] = segment

  final override def getValueForBound(bound: Bound[E]): V = value

  final override def getValueForExtended(bound: ExtendedBound[E]): V = value

  final override def getValueForElement(element: E): V = value
  
  // Transformation ----------------------------------------------------------- //
  final override def takeAboveBound(bound: Bound[E]): UniformSegmentSeq[E, D, V] = this

  final override def takeAboveExtended(bound: ExtendedBound[E]): UniformSegmentSeq[E, D, V] = this

  final override def takeBelowBound(bound: Bound[E]): UniformSegmentSeq[E, D, V] = this

  final override def takeBelowExtended(bound: ExtendedBound[E]): UniformSegmentSeq[E, D, V] = this

  final override def sliceAtBound(bound: Bound[E]): (UniformSegmentSeq[E, D, V], UniformSegmentSeq[E, D, V]) =
    (this, this)

  final override def sliceAtExtended(
    bound: ExtendedBound[E]
  ): (UniformSegmentSeq[E, D, V], UniformSegmentSeq[E, D, V]) =
    (this, this)

  final override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = other
  
  final override def prependBelowBound(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
    val upperBound = bound.provideUpper

    val otherBoundSegment = other.getSegmentForBound(upperBound)

    val leftSequence = otherBoundSegment.takeBelow
    val rightSequence = consPrepended(bound, otherBoundSegment.value)

    if (rightSequence.isUniform) leftSequence
    else rightSequence.prependBelowBound(bound, leftSequence)
  }

  final override def prependBelowExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    super.prependBelowExtended(bound, other)

  final override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = other

  final override def appendAboveBound(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
    val lowerBound = bound.provideLower

    val otherBoundSegment = other.getSegmentForBound(lowerBound)

    val leftSequence = consAppended(bound, otherBoundSegment.value)
    val rightSequence = otherBoundSegment.takeAbove

    if (leftSequence.isUniform) rightSequence
    else leftSequence.appendAboveBound(bound, rightSequence)
  }

  final override def appendAboveExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    super.appendAboveExtended(bound, other)

  final override def patchLazy(supplierSeq: SupplierSegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    patchLazyDefaultInternal(supplierSeq)
  
  // Protected section -------------------------------------------------------- //
  protected final override type SegmentInternal = UniformSingleSegment[E, D, V]

  /** Single segment instance. */
  protected val segment: UniformSingleSegment[E, D, V] = UniformSingleSegment(this)

  /** Value of single segment. */
  protected val value: V
  
  protected override def isValueIncluded(value: V): Boolean

  protected override def consUniform(value: V): UniformSegmentSeq[E, D, V]

  /**
   * Creates segment sequence:
   * <div>(minBound, U(bound)) -> `firstValue`</div>
   * <div>(L(bound), maxBound) -> `this.value`</div>
   * <div>where</div>
   * <div>minBound - minimal bound of domain;</div>
   * <div>maxBound - maximal bound of domain;</div>
   * <div>
   *   U - upper bound operator, it acts as identity if bound is upper and flips bound otherwise
   *   (see [[Bound.provideUpper]]);
   * </div>
   * <div>
   *   L - lower bound operator, it acts as identity if bound is lower and flips bound otherwise
   *   (see [[Bound.provideLower]]).
   * </div>
   * <div></div>
   * If `firstValue` and `this.value` are equals returns current uniform sequence (bound in result sequence is dropped).
   */
  protected def consPrepended(bound: Bound[E], firstValue: V): SegmentSeq[E, D, V]

  /**
   * Creates segment sequence:
   * <div>(minBound, U(bound)) -> `this.value`</div>
   * <div>(L(bound), maxBound) -> `lastValue`</div>
   * <div>where</div>
   * <div>minBound - minimal bound of domain;</div>
   * <div>maxBound - maximal bound of domain;</div>
   * <div>
   *   U - upper bound operator, it acts as identity if bound is upper and flips bound otherwise
   *   (see [[Bound.provideUpper]]);
   * </div>
   * <div>
   *   L - lower bound operator, it acts as identity if bound is lower and flips bound otherwise
   *   (see [[Bound.provideLower]]).
   * </div>
   * <div></div>
   * If `this.value` and `lastValue` are equals returns current uniform sequence (bound in result sequence is dropped).
   */
  protected def consAppended(bound: Bound[E], lastValue: V): SegmentSeq[E, D, V]

  protected final override def prependBelowBoundInternal(
    bound: Bound[E],
    originalBoundSegment: UniformSingleSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] = 
    prependBelowBound(bound, other)

  protected final override def appendAboveBoundInternal(
    bound: Bound[E],
    originalBoundSegment: UniformSingleSegment[E, D, V],
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] = 
    appendAboveBound(bound, other)
}

object AbstractUniformSegmentSeq {

  type UniformTruncation[E, D[X] <: Domain[X], V] = UniformSingleSegment.Truncation[E, D, V, UniformSingleSegment[E, D, V]]
  
  /**
   * Single segment of sequence. It has no previous and next segments.
   */
  final case class UniformSingleSegment[E, D[X] <: Domain[X], V](
    override val sequence: AbstractUniformSegmentSeq[E, D, V]
  ) extends SegmentT.Single[E, D, V, UniformSingleSegment[E, D, V]] {

    // Inspection --------------------------------------------------------------- //
    override def value: V = sequence.value

    override def isIncluded: Boolean = sequence.isValueIncluded(value)

    override def self: UniformSingleSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: UniformSingleSegment[E, D, V] = this

    override def moveToLast: UniformSingleSegment[E, D, V] = this

    override def moveToBound(bound: Bound[E]): UniformSingleSegment[E, D, V] = this

    override def moveToExtended(bound: ExtendedBound[E]): UniformSingleSegment[E, D, V] = this

    override def moveToElement(element: E): UniformSingleSegment[E, D, V] = this

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: UniformSegmentSeq[E, D, V] = sequence

    override def takeBelow: UniformSegmentSeq[E, D, V] = sequence

    override def slice: (UniformSegmentSeq[E, D, V], UniformSegmentSeq[E, D, V]) =
      (sequence, sequence)

    override def prepend(other: SegmentSeq[E, D, V]): UniformSegmentSeq[E, D, V] = sequence

    override def append(other: SegmentSeq[E, D, V]): UniformSegmentSeq[E, D, V] = sequence

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, UniformSingleSegment[E, D, V], this.type] =
      new UniformSingleSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, UniformSingleSegment[E, D, V], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, UniformSingleSegment[E, D, V], this.type] =
      SegmentTruncationT.upperTruncation(this)
  }

  object UniformSingleSegment {

    final class Truncation[E, D[X] <: Domain[X], V, +Seg <: UniformSingleSegment[E, D, V]](
      override val segment: Seg,
      inputBound: ExtendedBound[E],
    ) extends SegmentT.Single.Truncation[E, D, V, UniformSingleSegment[E, D, V], Seg](
      segment,
      inputBound,
    ) {

      override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
        segment.sequence.prependBelowExtended(bound, other)

      override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
        segment.sequence.appendAboveExtended(bound, other)
    }
  }
}
