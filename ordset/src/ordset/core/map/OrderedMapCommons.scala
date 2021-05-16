package ordset.core.map

import ordset.core.SegmentSeq
import ordset.core.domain.Domain

trait OrderedMapCommons[E, D <: Domain[E], V] {
  self: SegmentSeq[E, D, V] =>
}
