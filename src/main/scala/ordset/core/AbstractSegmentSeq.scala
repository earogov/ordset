package ordset.core

import ordset.core.domain.Domain

abstract class AbstractSegmentSeq[E, D <: Domain[E],  W] extends SegmentSeq[E, D, W] {

  // Protected section -------------------------------------------------------- //
  protected final type GenSegment = Segment[E, D, W]
  protected final type FirstSegment = Segment.First[E, D, W]
  protected final type LastSegment = Segment.Last[E, D, W]
  protected final type InitialSegment = Segment.Initial[E, D, W]
  protected final type TerminalSegment = Segment.Terminal[E, D, W]
  protected final type InnerSegment = Segment.Inner[E, D, W]
  protected final type SingleSegment = Segment.Single[E, D, W]
  protected final type SegmentWithNext = Segment.WithNext[E, D, W]
  protected final type SegmentWithPrev = Segment.WithPrev[E, D, W]

  /**
   * Get collection of upper bounds that not less then upper bound of specified segment.
   */
  protected def forwardUpperBoundsFromSegment(segment: Segment[E, D, W]): Iterable[Bound.Upper[E]] =
    segment.forwardIterable.map {
      case s: SegmentWithNext => s.upperBound
      case _ => null
    }.filterNot(_ == null)
}
