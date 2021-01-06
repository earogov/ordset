package ordset

import ordset.domain.{Domain, DomainOps}

// TODO: class description.
abstract class AbstractUniformSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>

  // Navigation --------------------------------------------------------------- //
  final override def isEmpty: Boolean = !belongsToSet(value)

  final override def isUniversal: Boolean = belongsToSet(value)

  final override def contains(bound: Bound[E]): Boolean = belongsToSet(value)

  final override def contains(element: E): Boolean = belongsToSet(value)

  // Navigation --------------------------------------------------------------- //
  final override def firstSegment: Segment.First[E, D, W] = UniformSingleSegment

  final override def lastSegment: Segment.Last[E, D, W] = UniformSingleSegment

  final override def getSegment(bound: Bound[E]): Segment[E, D, W] = UniformSingleSegment

  final override def getSegment(element: E): Segment[E, D, W] = UniformSingleSegment

  // Transformation ----------------------------------------------------------- //
  final override def droppedBelow(bound: Bound[E]): SegmentSeq[E, D, W] = this

  final override def droppedAbove(bound: Bound[E]): SegmentSeq[E, D, W] = this

  final override def slice(bound: Bound[E]): (SegmentSeq[E, D, W], SegmentSeq[E, D, W]) = (this, this)

  final override def appended(other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W] = other

  // Protected section -------------------------------------------------------- //
  /** Value of single segment. */
  protected val value: W

  /**
   * @return true if `value` belongs to set.
   */
  protected def belongsToSet(value: W): Boolean

  /**
   * Single segment of sequence. It has no previous and next segments.
   */
  protected case object UniformSingleSegment extends SingleSegment {

    override def domainOps: DomainOps[E, D] = seq.domainOps

    override def value: W = seq.value
  }
}
