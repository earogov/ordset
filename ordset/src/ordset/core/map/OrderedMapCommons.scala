package ordset.core.map

import ordset.core.SegmentSeq
import ordset.core.domain.Domain
import ordset.core.value.ValueOps

trait OrderedMapCommons[E, D <: Domain[E], W] {
  self: SegmentSeq[E, D, W] =>
  
  @inline
  final override def isSet: Boolean = false
}
