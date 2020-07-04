package ordset

class ZippedOrderedSet[E, D <: Domain[E]](
  override val left: SetSegmentSeq[E, D],
  override val right: SetSegmentSeq[E, D],
  val operatorFn: (Boolean, Boolean) => Boolean,
  val invariantFn: Boolean => Boolean
)(
  implicit override val domain: D
) extends AbstractZippedSegmentSeq[E, D, Boolean] {

  override final val valueEq: Eq[Boolean] = booleanEq

  override protected def operator(left: Boolean, right: Boolean): Boolean = operatorFn(left, right)

  override protected def invariant(value: Boolean): Boolean = invariantFn(value)

  override protected def belongsToSet(value: Boolean): Boolean = value
}

object ZippedOrderedSet {

  def union[E, D <: Domain[E]](left: SetSegmentSeq[E, D], right: SetSegmentSeq[E, D])(implicit domain: D
  ): ZippedOrderedSet[E, D] = new ZippedOrderedSet[E, D](left, right, _ || _, x => x)

  def intersection[E, D <: Domain[E]](left: SetSegmentSeq[E, D], right: SetSegmentSeq[E, D])(implicit domain: D
  ): ZippedOrderedSet[E, D] = new ZippedOrderedSet[E, D](left, right, _ && _, !_)
}