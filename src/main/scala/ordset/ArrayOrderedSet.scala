package ordset

import ordset.domain.{Domain, DomainOps}

import scala.collection.immutable.ArraySeq

sealed class ArrayOrderedSet[E, D <: Domain[E]] (
  protected final val bounds: ArraySeq[Bound.Upper[E]],
  protected final val complement: Boolean
)(
  implicit override final val domainOps: DomainOps[E, D]
) extends ArraySegmentSeq[E, D, Boolean] {

  @inline
  override protected final def getSegmentValue(ind: Int): Boolean = belongsToSet(ind)
}
