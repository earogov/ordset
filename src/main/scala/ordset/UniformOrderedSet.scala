package ordset

import ordset.domain.{Domain, DomainOps}

class UniformOrderedSet[E, D <: Domain[E]](
  final override val value: Boolean
)(
  implicit final override val domainOps: DomainOps[E, D]
) extends AbstractUniformSegmentSeq[E, D, Boolean] {

  @inline
  protected final override def belongsToSet(value: Boolean): Boolean = value
}

object UniformOrderedSet {

  def apply[E, D <: Domain[E]](value: Boolean)(implicit domainOps: DomainOps[E, D]): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](value)(domainOps)

  def empty[E, D <: Domain[E]](implicit domainOps: DomainOps[E, D]): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](false)(domainOps)

  def universal[E, D <: Domain[E]](implicit domainOps: DomainOps[E, D]): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](true)(domainOps)
}
