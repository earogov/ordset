package ordset.core

import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps, OrderValidationFunc}
import ordset.core.set.TreapOrderedSet

// TODO: class description.
abstract class AbstractUniformSegmentSeq[E, D <: Domain[E],  V]
  extends AbstractSegmentSeq[E, D, V, AbstractUniformSegmentSeq.UniformSingleSegment[E, D, V]] {
  seq =>

  import AbstractUniformSegmentSeq._

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
  final override def takenAbove(bound: Bound[E]): AbstractUniformSegmentSeq[E, D, V] = this

  final override def takenBelow(bound: Bound[E]): AbstractUniformSegmentSeq[E, D, V] = this

  final override def sliced(bound: Bound[E]): (AbstractUniformSegmentSeq[E, D, V], AbstractUniformSegmentSeq[E, D, V]) =
    (this, this)
  
  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
    val lowerBound = bound.provideLower
    
    val otherBoundSegment = other.getSegment(lowerBound)

    val leftSequence = consBounded(bound, otherBoundSegment.value)
    val rightSequence = otherBoundSegment.takenAbove
    
    if (leftSequence.isUniform) rightSequence
    else leftSequence.appended(bound, rightSequence)
  }

  // Protected section -------------------------------------------------------- //
  /** Single segment instance. */
  protected val segment: UniformSingleSegment[E, D, V] = new UniformSingleSegment(this)

  /** Value of single segment. */
  protected val value: V

  /**
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `V` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isValueIncluded(value: V): Boolean = valueOps.isIncluded(value)

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
  protected def consBounded(bound: Bound[E], lastValue: V): SegmentSeq[E, D, V]
}

object AbstractUniformSegmentSeq {

  /**
   * Single segment of sequence. It has no previous and next segments.
   */
  final case class UniformSingleSegment[E, D <: Domain[E], V](
    override val sequence: AbstractUniformSegmentSeq[E, D, V]
  ) extends SegmentT.Single[E, D, V, UniformSingleSegment[E, D, V]] {

    // Inspection --------------------------------------------------------------- //
    override def value: V = sequence.value

    override def isIncluded: Boolean = sequence.isValueIncluded(value)

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: UniformSingleSegment[E, D, V] = this

    override def moveToLast: UniformSingleSegment[E, D, V] = this

    override def moveTo(bound: Bound[E]): UniformSingleSegment[E, D, V] = this

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: AbstractUniformSegmentSeq[E, D, V] = sequence

    override def takenBelow: AbstractUniformSegmentSeq[E, D, V] = sequence

    override def sliced: (AbstractUniformSegmentSeq[E, D, V], AbstractUniformSegmentSeq[E, D, V]) =
      (sequence, sequence)

    override def appended(other: SegmentSeq[E, D, V]): AbstractUniformSegmentSeq[E, D, V] = sequence

    // Protected section -------------------------------------------------------- //
    protected override def self: UniformSingleSegment[E, D, V] = this
  }
}
