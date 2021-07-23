package ordset.core.map

import ordset.core.AbstractSegmentSeq
import ordset.core.domain.Domain

trait OrderedMapCommons[E, D <: Domain[E], V, +S] {
  self: AbstractSegmentSeq[E, D, V, S] =>

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def isValueIncluded(value: V): Boolean = valueOps.isIncluded(value)
}
