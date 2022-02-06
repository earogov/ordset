package ordset.core.segmentSeq

import ordset.core.{Bound, ExtendedBound}
import ordset.core.domain.Domain
import ordset.core.segmentSeq.AbstractMappedSegmentSeq
import ordset.core.segmentSeq.internal.TransformationUtil
import AbstractMappedSegmentSeq._

abstract class AbstractMappedValueSegmentSeq[E, D[X] <: Domain[X], U, V, S]
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
  ): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = {
    val originalSlice = TransformationUtil.sliceComposedSegment(segment.back, segment.front)
    (cons(originalSlice._1), cons(originalSlice._2))
  }
}

