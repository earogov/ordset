package ordset.core.segmentSeq.map

import ordset.core.segmentSeq.AbstractSegmentSeq
import ordset.core.domain.Domain

trait OrderedMapCommons[E, D[X] <: Domain[X], V, +S] {
  self: AbstractSegmentSeq[E, D, V, S] =>

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def isValueIncluded(value: V): Boolean = valueOps.isIncluded(value)
}
