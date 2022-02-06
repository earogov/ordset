package ordset.core.segmentSeq.internal

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.{Segment, SegmentSeq}

protected[ordset] object TransformationUtil {
  
  /**
   * Some types of segment sequences may wrap another sequences and merge several their segments into one (mapped,
   * zipped, etc.). Let's denote such segments as composed.
   * {{{
   * 
   *         `back`          `front`
   *       A      \  B     C /       D
   * X----------)[-----)[-----](-----------X  original sequence
   *             |            |
   *       A     |      B     |      C
   * X----------)[------------](-----------X  mapped sequence
   * }}}
   * <tr></tr>
   * 
   * Method returns slice of original sequence, i.e. tuple:
   * {{{
   *   (back.takeBelow, front.takeAbove)
   * }}}
   * <tr></tr>
   * 
   * Preconditions:
   * <tr>
   *   1. `back` must define lower bound of some composed segment and `front` - upper bound of the same segment.
   * </tr>
   */ 
  def sliceComposedSegment[E, D[X] <: Domain[X], V](
      back: Segment[E, D, V],
      front: Segment[E, D, V]
  ): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) =
    // If `front` and `back` is the same segment we can do an optimization:
    // treap based original sequence, for example, can build both left and right slice parts simultaneously,
    // so we should use `segment.slice` whenever possible.
    if (front.domainOps.segments.upperOrd.eqv(back, front)) front.slice
    else (back.takeBelow, front.takeAbove)
}
