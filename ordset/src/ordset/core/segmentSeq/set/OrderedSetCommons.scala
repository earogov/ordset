package ordset.core.segmentSeq.set

import ordset.core.value.ValueOps
import ordset.core.segmentSeq.AbstractSegmentSeq
import ordset.core.domain.Domain

trait OrderedSetCommons[E, D[X] <: Domain[X], +S] {
  self: AbstractSegmentSeq[E, D, Boolean, S] => 

  @inline
  final override def valueOps: ValueOps[Boolean] = ValueOps.booleanValueOps

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def isValueIncluded(value: Boolean): Boolean = value
}
