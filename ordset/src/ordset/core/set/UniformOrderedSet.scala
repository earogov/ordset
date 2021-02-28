package ordset.core.set

import ordset.core.AbstractUniformSegmentSeq
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager

class UniformOrderedSet[E, D <: Domain[E]](
  final override val value: Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractUniformSegmentSeq[E, D, Boolean]
  with OrderedSetCommons[E, D] {

  @inline
  protected final override def isIncludedInSet(value: Boolean): Boolean = value
}

object UniformOrderedSet {

  def apply[E, D <: Domain[E]](
    value: Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](value)

  def empty[E, D <: Domain[E]](
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](false)

  def universal[E, D <: Domain[E]](
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](true)
}
