package ordset.core.set

import ordset.Eq
import ordset.core.SegmentSeq
import ordset.core.domain.Domain

trait OrderedSetCommons[E, D <: Domain[E]] {
  self: SegmentSeq[E, D, Boolean] =>

  @inline
  final override def valueEq: Eq[Boolean] = ordset.instances.boolean.booleanHash
}
