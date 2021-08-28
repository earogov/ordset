package ordset.core

import ordset.core.domain.Domain
import ordset.core.AbstractMappedSegmentSeq

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
}

