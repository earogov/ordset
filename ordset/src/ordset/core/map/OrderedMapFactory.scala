package ordset.core.map

import ordset.core.domain.Domain
import ordset.core.{Bound, OrderedMap, SegmentSeqException}

trait OrderedMapFactory[E, D <: Domain[E], V] {

  @throws[SegmentSeqException]("if unable to build valid map with specified bounds")
  def buildUnsafe(bounds: IterableOnce[Bound.Upper[E]], values: IterableOnce[V]): OrderedMap[E, D, V]
}
