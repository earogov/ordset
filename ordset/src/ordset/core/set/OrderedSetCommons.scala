package ordset.core.set

import ordset.core.value.ValueOps
import ordset.core.SegmentSeq
import ordset.core.domain.Domain

trait OrderedSetCommons[E, D <: Domain[E]] {
  self: SegmentSeq[E, D, Boolean] =>

  @inline
  final override def valueOps: ValueOps[Boolean] = ValueOps.booleanValueOps
}
