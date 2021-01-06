package ordset

import ordset.domain.{Domain, DomainOps}

import scala.collection.immutable.ArraySeq

class ArrayOrderedSet[E, D <: Domain[E]] protected (
  final val bounds: ArraySeq[Bound.Upper[E]],
  final val complement: Boolean
)(
  implicit final override val domainOps: DomainOps[E, D]
) extends AbstractArraySegmentSeq[E, D, Boolean] {

  validate()

  @inline
  protected final override def getSegmentValue(ind: Int): Boolean = belongsToSet(ind)
}

object ArrayOrderedSet {

  def apply[E, D <: Domain[E]](
    bounds: ArraySeq[Bound.Upper[E]],
    complement: Boolean
  )(
    implicit domainOps: DomainOps[E, D]
  ): OrderedSet[E, D] =
    if (bounds.isEmpty) UniformOrderedSet(complement)(domainOps)
    else new ArrayOrderedSet[E, D](bounds, complement)(domainOps)
}