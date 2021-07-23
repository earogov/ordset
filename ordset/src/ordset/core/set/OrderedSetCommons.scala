package ordset.core.set

import ordset.core.value.ValueOps
import ordset.core.AbstractSegmentSeq
import ordset.core.domain.Domain

trait OrderedSetCommons[E, D <: Domain[E], +S] {
  self: AbstractSegmentSeq[E, D, Boolean, S] => 

  @inline
  final override def valueOps: ValueOps[Boolean] = ValueOps.booleanValueOps

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def isValueIncluded(value: Boolean): Boolean = value
}
