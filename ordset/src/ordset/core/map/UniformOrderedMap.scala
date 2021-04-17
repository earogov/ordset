package ordset.core.map

import ordset.core.value.ValueOps
import ordset.core.{AbstractUniformSegmentSeq, Bound, SegmentSeq}
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager

class UniformOrderedMap[E, D <: Domain[E], W](
  final override val value: W
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val valueOps: ValueOps[W],
  final override val rngManager: RngManager
) extends AbstractUniformSegmentSeq[E, D, W] {

  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W] = ???
  
  @inline
  protected final override def isIncludedInSet(value: W): Boolean = valueOps.isIncluded(value)
}

object UniformOrderedMap {

  def apply[E, D <: Domain[E], W](
    value: W
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[W],
    rngManager: RngManager
  ): UniformOrderedMap[E, D, W] =
    new UniformOrderedMap[E, D, W](value)
}
