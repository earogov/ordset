package ordset

import ordset.domain.{Domain, DomainOps}

class UniformOrderedSet[E, D <: Domain[E]](
  override final val value: Boolean
)(
  implicit override final val domainOps: DomainOps[E, D]
) extends AbstractUniformSegmentSeq[E, D, Boolean] {

  @inline
  override protected final def belongsToSet(value: Boolean): Boolean = value
}

object UniformOrderedSet {

  def empty[E, D <: Domain[E]](implicit domainOps: DomainOps[E, D]): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](false)(domainOps)

  def universal[E, D <: Domain[E]](implicit domainOps: DomainOps[E, D]): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](true)(domainOps)
}
