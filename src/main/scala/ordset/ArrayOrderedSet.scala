package ordset

import ordset.domain.{Domain, DomainOps}

import scala.collection.immutable.ArraySeq

sealed class ArrayOrderedSet[E, D <: Domain[E]] (
  protected val bounds: ArraySeq[Bound.Upper[E]],
  protected val complement: Boolean
)(
  implicit override val domainOps: DomainOps[E, D]
) extends ArraySegmentSeq[E, D, Boolean] {

  override protected def getSegmentValue(ind: Int): Boolean = belongsToSet(ind)
}
