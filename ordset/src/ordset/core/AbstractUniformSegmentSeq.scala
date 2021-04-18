package ordset.core

import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps, OrderValidationFunc}
import ordset.core.set.TreapOrderedSet

// TODO: class description.
abstract class AbstractUniformSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>

  import AbstractUniformSegmentSeq._

  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = !isIncludedInSet(value)

  final override def isUniversal: Boolean = isIncludedInSet(value)

  final override def isUniform: Boolean = true

  final override def contains(bound: Bound[E]): Boolean = isUniversal

  final override def contains(element: E): Boolean = isUniversal

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = Iterable.empty

  final override def firstSegment: UniformSingleSegment[E, D, W] = segment

  final override def lastSegment: UniformSingleSegment[E, D, W] = segment

  final override def getSegment(bound: Bound[E]): UniformSingleSegment[E, D, W] = segment

  final override def getSegment(element: E): UniformSingleSegment[E, D, W] = segment

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): AbstractUniformSegmentSeq[E, D, W] = this

  final override def takenBelow(bound: Bound[E]): AbstractUniformSegmentSeq[E, D, W] = this

  final override def sliced(bound: Bound[E]): (AbstractUniformSegmentSeq[E, D, W], AbstractUniformSegmentSeq[E, D, W]) =
    (this, this)
  
  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W] = {
    val lowerBound = bound.provideLower
    
    val otherBoundSegment = other.getSegment(lowerBound)

    val leftSequence = consBounded(bound, otherBoundSegment.value)
    val rightSequence = otherBoundSegment.takenAbove
    
    if (leftSequence.isUniform) rightSequence
    else leftSequence.appended(bound, rightSequence)
  }

  // Protected section -------------------------------------------------------- //
  /** Single segment instance. */
  protected val segment: UniformSingleSegment[E, D, W] = new UniformSingleSegment(this)

  /** Value of single segment. */
  protected val value: W

  /**
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `W` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isIncludedInSet(value: W): Boolean = valueOps.isIncluded(value)

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
  protected def consBounded(bound: Bound[E], lastValue: W): SegmentSeq[E, D, W]
}

object AbstractUniformSegmentSeq {

  /**
   * Single segment of sequence. It has no previous and next segments.
   */
  final case class UniformSingleSegment[E, D <: Domain[E], W](
    override val sequence: AbstractUniformSegmentSeq[E, D, W]
  ) extends Segment.Single[E, D, W] {

    // Inspection --------------------------------------------------------------- //
    override def value: W = sequence.value

    override def isIncluded: Boolean = sequence.isIncludedInSet(value)

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: AbstractUniformSegmentSeq[E, D, W] = sequence

    override def takenBelow: AbstractUniformSegmentSeq[E, D, W] = sequence

    override def sliced: (AbstractUniformSegmentSeq[E, D, W], AbstractUniformSegmentSeq[E, D, W]) =
      (sequence, sequence)
  }
}
