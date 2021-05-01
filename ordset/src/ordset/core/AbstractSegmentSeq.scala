package ordset.core

import ordset.core.domain.Domain

abstract class AbstractSegmentSeq[E, D <: Domain[E], W, +S] extends SegmentSeqT[E, D, W, S] {

  // Protected section -------------------------------------------------------- //
  /**
   * Get collection of upper bounds that not less then upper bound of specified segment.
   */
  protected def forwardUpperBoundsFromSegment(segment: Segment[E, D, W]): Iterable[Bound.Upper[E]] =
    segment.forwardIterable().map {
      case s: Segment.WithNext[E, D, W] => s.upperBound
      case _ => null
    }.filterNot(_ == null)
}
