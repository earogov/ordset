package ordset

import ordset.domain.{Domain, DomainOps}

// TODO: class description.
abstract class AbstractUniformSegmentSeq[E, D <: Domain[E],  W] extends AbstractSegmentSeq[E, D, W] { seq =>

  final override def isEmpty: Boolean = !belongsToSet(value)

  /** @return true if sequence is universal i.e. contains all elements of domain. */
  final override def isUniversal: Boolean = belongsToSet(value)

  /** @return true if sequence contains `bound`. */
  final override def contains(bound: Bound[E]): Boolean = belongsToSet(value)

  /** @return true if sequence contains `element`. */
  final override def contains(element: E): Boolean = belongsToSet(value)

  /** @return first segment of sequence. */
  final override def firstSegment: Segment.First[E, D, W] = UniformSingleSegment

  /** @return last segment of sequence. */
  final override def lastSegment: Segment.Last[E, D, W] = UniformSingleSegment

  /** @return segment which contains specified `bound`. */
  final override def getSegment(bound: Bound[E]): Segment[E, D, W] = UniformSingleSegment

  /** @return segment which contains specified `element`. */
  final override def getSegment(element: E): Segment[E, D, W] = UniformSingleSegment

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
