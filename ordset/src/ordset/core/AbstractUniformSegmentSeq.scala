package ordset.core

import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.NonuniformTreapOrderedSet
import AbstractUniformSegmentSeq._

// TODO: class description.
abstract class AbstractUniformSegmentSeq[E, D <: Domain[E],  V]
  extends AbstractSegmentSeq[E, D, V, UniformSingleSegment[E, D, V]] {

  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = !isValueIncluded(value)

  final override def isUniversal: Boolean = isValueIncluded(value)

  final override def isUniform: Boolean = true

  final override def contains(bound: Bound[E]): Boolean = isUniversal

  final override def containsElement(element: E): Boolean = isUniversal

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = Iterable.empty

  final override def firstSegment: UniformSingleSegment[E, D, V] = segment

  final override def lastSegment: UniformSingleSegment[E, D, V] = segment

  final override def getSegment(bound: Bound[E]): UniformSingleSegment[E, D, V] = segment

  final override def getSegmentForElement(element: E): UniformSingleSegment[E, D, V] = segment

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): UniformSegmentSeq[E, D, V] = this

  final override def takenBelow(bound: Bound[E]): UniformSegmentSeq[E, D, V] = this

  final override def sliced(bound: Bound[E]): (UniformSegmentSeq[E, D, V], UniformSegmentSeq[E, D, V]) =
    (this, this)

  final override def prepended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = other
  
  final override def prepended(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
    val upperBound = bound.provideUpper

    val otherBoundSegment = other.getSegment(upperBound)

    val leftSequence = otherBoundSegment.takenBelow
    val rightSequence = consPrepended(bound, otherBoundSegment.value)

    if (rightSequence.isUniform) leftSequence
    else rightSequence.prepended(bound, leftSequence)
  }

  final override def appended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = other
  
  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
    val lowerBound = bound.provideLower
    
    val otherBoundSegment = other.getSegment(lowerBound)

    val leftSequence = consAppended(bound, otherBoundSegment.value)
    val rightSequence = otherBoundSegment.takenAbove
    
    if (leftSequence.isUniform) rightSequence
    else leftSequence.appended(bound, rightSequence)
  }

  // Protected section -------------------------------------------------------- //
  /** Single segment instance. */
  protected val segment: UniformSingleSegment[E, D, V] = UniformSingleSegment(this)

  /** Value of single segment. */
  protected val value: V

  /**
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `V` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isValueIncluded(value: V): Boolean

  /**
   * Creates segment sequence:
   * <tr>(minBound, U(bound)) -> `firstValue`</tr>
   * <tr>(L(bound), maxBound) -> `this.value`</tr>
   * <tr>where</tr>
   * <tr>minBound - minimal bound of domain;</tr>
   * <tr>maxBound - maximal bound of domain;</tr>
   * <tr>
   *   U - upper bound operator, it acts as identity if bound is upper and flips bound otherwise 
   *   (see [[Bound.provideUpper]]);
   * </tr>
   * <tr>
   *   L - lower bound operator, it acts as identity if bound is lower and flips bound otherwise 
   *   (see [[Bound.provideLower]]).
   * </tr>
   * <tr></tr>
   * If `firstValue` and `this.value` are equals returns current uniform sequence (bound in result sequence is dropped).
   */
  protected def consPrepended(bound: Bound[E], firstValue: V): SegmentSeq[E, D, V]
  
  /**
   * Creates segment sequence:
   * <tr>(minBound, U(bound)) -> `this.value`</tr>
   * <tr>(L(bound), maxBound) -> `lastValue`</tr>
   * <tr>where</tr>
   * <tr>minBound - minimal bound of domain;</tr>
   * <tr>maxBound - maximal bound of domain;</tr>
   * <tr>
   *   U - upper bound operator, it acts as identity if bound is upper and flips bound otherwise 
   *   (see [[Bound.provideUpper]]);
   * </tr>
   * <tr>
   *   L - lower bound operator, it acts as identity if bound is lower and flips bound otherwise 
   *   (see [[Bound.provideLower]]).
   * </tr>
   * <tr></tr>
   * If `this.value` and `lastValue` are equals returns current uniform sequence (bound in result sequence is dropped).
   */
  protected def consAppended(bound: Bound[E], lastValue: V): SegmentSeq[E, D, V]
}

object AbstractUniformSegmentSeq {

  /**
   * Single segment of sequence. It has no previous and next segments.
   */
  final case class UniformSingleSegment[E, D <: Domain[E], V](
    override val sequence: UniformSegmentSeq[E, D, V]
  ) extends SegmentT.Single[E, D, V, UniformSingleSegment[E, D, V]] {

    // Inspection --------------------------------------------------------------- //
    override def value: V = sequence.value

    override def isIncluded: Boolean = sequence.isValueIncluded(value)

    override def self: UniformSingleSegment[E, D, V] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: UniformSingleSegment[E, D, V] = this

    override def moveToLast: UniformSingleSegment[E, D, V] = this

    override def moveTo(bound: Bound[E]): UniformSingleSegment[E, D, V] = this

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: UniformSegmentSeq[E, D, V] = sequence

    override def takenBelow: UniformSegmentSeq[E, D, V] = sequence

    override def sliced: (UniformSegmentSeq[E, D, V], UniformSegmentSeq[E, D, V]) =
      (sequence, sequence)

    override def prepended(other: SegmentSeq[E, D, V]): UniformSegmentSeq[E, D, V] = sequence

    override def appended(other: SegmentSeq[E, D, V]): UniformSegmentSeq[E, D, V] = sequence

    override def truncation(bound: Bound[E]): UniformSingleSegment.Truncation[E, D, V] =
      new UniformSingleSegment.Truncation(this, bound)
  }

  object UniformSingleSegment {

    final class Truncation[E, D <: Domain[E], V](
      override val segment: UniformSingleSegment[E, D, V],
      inputBound: Bound[E],
    ) extends SegmentT.Single.Truncation[E, D, V, UniformSingleSegment[E, D, V]](
      segment,
      inputBound,
    ) {

      override def prepended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
        segment.sequence.prepended(bound, other)

      override def appended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
        segment.sequence.appended(bound, other)
    }
  }
}
