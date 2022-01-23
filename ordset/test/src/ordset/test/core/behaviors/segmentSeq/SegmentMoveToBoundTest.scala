package ordset.test.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.ExtendedBound
import ordset.core.interval.IntervalRelation

trait SegmentMoveToBoundTest[E, D[X] <: Domain[X], V] {

  def moveToBoundCases: Seq[(ExtendedBound[E], IntervalRelation[E, D, V])]
}
