package ordset

import scala.collection.immutable.ArraySeq

sealed class ArrayOrderedSet[E] (
  protected val bounds: ArraySeq[Bound.Upper[E]],
  protected val complement: Boolean
)(
  implicit
  override val domain: Domain[E],
) extends ArraySegmentSeq[E, Boolean] {

  override protected def getSegmentValue(ind: Int): Boolean = belongsToSet(ind)
}
