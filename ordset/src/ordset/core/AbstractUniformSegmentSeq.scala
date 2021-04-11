package ordset.core

import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps}

// TODO: class description.
abstract class AbstractUniformSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>

  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean = !isIncludedInSet(value)

  final override def isUniversal: Boolean = isIncludedInSet(value)

  final override def isUniform: Boolean = true

  final override def contains(bound: Bound[E]): Boolean = isUniversal

  final override def contains(element: E): Boolean = isUniversal

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = Iterable.empty

  final override def firstSegment: UniformSingleSegment.type = UniformSingleSegment

  final override def lastSegment: UniformSingleSegment.type = UniformSingleSegment

  final override def getSegment(bound: Bound[E]): UniformSingleSegment.type = UniformSingleSegment

  final override def getSegment(element: E): UniformSingleSegment.type = UniformSingleSegment

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): AbstractUniformSegmentSeq[E, D, W] = this

  final override def takenBelow(bound: Bound[E]): AbstractUniformSegmentSeq[E, D, W] = this

  final override def sliced(bound: Bound[E]): (AbstractUniformSegmentSeq[E, D, W], AbstractUniformSegmentSeq[E, D, W]) =
    (this, this)
  
  final override def appended(other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W] = other

  // Protected section -------------------------------------------------------- //
  /** Value of single segment. */
  protected val value: W

  /**
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `W` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isIncludedInSet(value: W): Boolean

  /**
   * Single segment of sequence. It has no previous and next segments.
   */
  protected case object UniformSingleSegment extends SingleSegment {

    override def sequence: SegmentSeq[E, D, W] = seq
    
    override def domainOps: DomainOps[E, D] = seq.domainOps

    override def valueOps: ValueOps[W] = seq.valueOps

    override def value: W = seq.value

    override def isIncluded: Boolean = isIncludedInSet(value)
  }
}
