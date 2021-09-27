package ordset.core

import ordset.core.domain.Domain
import ordset.core.AbstractMappedSegmentSeq
import AbstractMappedSegmentSeq._

abstract class AbstractMappedValueSegmentSeq[E, D <: Domain[E], U, V, S]
  extends AbstractMappedSegmentSeq[E, D, U, V, S] {

  // Inspection --------------------------------------------------------------- //
  val valueMapFunc: U => V

  /** Mapping function for segment. */
  final override val segmentMapFunc: Segment[E, D, U] => V = s => valueMapFunc(s.value)

  // Navigation --------------------------------------------------------------- //
  final override def getValueForBound(bound: Bound[E]): V =
    valueMapFunc(originalSeq.getValueForBound(bound))

  final override def getValueForExtended(bound: ExtendedBound[E]): V =
    valueMapFunc(originalSeq.getValueForExtended(bound))

  final override def getValueForElement(element: E): V =
    valueMapFunc(originalSeq.getValueForElement(element))

  // Protected section -------------------------------------------------------- //
  final override protected def takeAboveInternal(segment: MappedInnerSegment[E, D, U, V, S]): SegmentSeq[E, D, V] =
    cons(segment.front.takeAbove)

  final override protected def takeBelowInternal(segment: MappedInnerSegment[E, D, U, V, S]): SegmentSeq[E, D, V] =
    cons(segment.back.takeBelow)

  final override protected def sliceInternal(
    segment: MappedInnerSegment[E, D, U, V, S]
  ): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) =
    if (domainOps.segmentUpperOrd.eqv(segment.front, segment.back)) {
       // If `front` and `back` are the same segment we can do an optimization:
       // treap based original sequence, for example, can build both left and right slice parts simultaneously,
       // so we should use `original.slice` whenever possible.
       val originalSlice = segment.front.slice
       (cons(originalSlice._1), cons(originalSlice._2))
     } else {
       (takeBelowInternal(segment), takeAboveInternal(segment))
     }
}

