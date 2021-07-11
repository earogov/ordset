package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{ExtendedBound, IntervalRelation}

trait SegmentMoveToBoundTest[E, D <: Domain[E], V] {

  def moveToBoundCases: Seq[(ExtendedBound[E], IntervalRelation[E, D, V])]
}
