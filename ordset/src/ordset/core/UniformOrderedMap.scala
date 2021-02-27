package ordset.core

import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager

class UniformOrderedMap[E, D <: Domain[E], W](
  final override val value: W,
  final val inclusionFunc: W => Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractUniformSegmentSeq[E, D, W] {

  @inline
  protected final override def isIncludedInSet(value: W): Boolean = inclusionFunc(value)
}

object UniformOrderedMap {

  def apply[E, D <: Domain[E], W](
    value: W,
    inclusionFunc: W => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedMap[E, D, W] =
    new UniformOrderedMap[E, D, W](value, inclusionFunc)
}
