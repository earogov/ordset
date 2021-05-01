package ordset.core.map

import ordset.core.SegmentSeq
import ordset.core.domain.Domain
import ordset.core.value.ValueOps

trait OrderedMapCommons[E, D <: Domain[E], V] {
  self: SegmentSeq[E, D, V] =>
  
  @inline
  final override def isSet: Boolean = false
}
