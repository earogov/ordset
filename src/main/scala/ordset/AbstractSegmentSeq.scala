package ordset

import ordset.domain.Domain

abstract class AbstractSegmentSeq[E, D <: Domain[E],  W] extends SegmentSeq[E, D, W] {

  protected final type GenSegment = Segment[E, D, W]
  protected final type FirstSegment = Segment.First[E, D, W]
  protected final type LastSegment = Segment.Last[E, D, W]
  protected final type InitialSegment = Segment.Initial[E, D, W]
  protected final type TerminalSegment = Segment.Terminal[E, D, W]
  protected final type InnerSegment = Segment.Inner[E, D, W]
  protected final type SingleSegment = Segment.Single[E, D, W]
  protected final type SegmentWithNext = Segment.WithNext[E, D, W]
  protected final type SegmentWithPrev = Segment.WithPrev[E, D, W]
}
