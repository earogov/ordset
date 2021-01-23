package test.ordset.core.behaviors

import ordset.core.domain.Domain
import ordset.core.{Bound, IntervalRelation}

trait SegmentMoveToBoundTest[E, D <: Domain[E], V] {

  def moveToBoundSeq: Seq[(Bound[E], IntervalRelation[E, D, V])]
}
