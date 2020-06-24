//package ordset
//
//class ZippedOrderedSet[E](
//  override val left: SetSegmentSeq[E],
//  override val right: SetSegmentSeq[E],
//  val operatorFn: (Boolean, Boolean) => Boolean,
//  val invariantFn: Boolean => Boolean
//)(
//  implicit override val domain: Domain[E]
//) extends AbstractZippedSegmentSeq[E, Boolean] {
//
//  override final val valueEq: Eq[Boolean] = booleanEq
//
//  override protected def operator(left: Boolean, right: Boolean): Boolean = operatorFn(left, right)
//
//  override protected def invariant(value: Boolean): Boolean = invariantFn(value)
//
//  /** @return true if sequence is empty i.e. contains no elements. */
//  override def isEmpty: Boolean = _firstSegment.isSingle && !_firstSegment.value
//
//  /** @return true if sequence is universal i.e. contains all elements of domain. */
//  override def isUniversal: Boolean = _firstSegment.isSingle && _firstSegment.value
//
//  /** @return true if sequence contains `bound`. */
//  override def contains(bound: Bound[E]): Boolean = getSegmentValue(left.getSegment(bound), right.getSegment(bound))
//
//  /** @return true if sequence contains `element`. */
//  override def contains(element: E): Boolean = getSegmentValue(left.getSegment(element), right.getSegment(element))
//}
//
//object ZippedOrderedSet {
//
//  def union[E](left: SetSegmentSeq[E], right: SetSegmentSeq[E])(implicit domain: Domain[E]): ZippedOrderedSet[E] =
//    new ZippedOrderedSet[E](left, right, _ || _, x => x)
//
//  def intersection[E](left: SetSegmentSeq[E], right: SetSegmentSeq[E])(implicit domain: Domain[E]): ZippedOrderedSet[E] =
//    new ZippedOrderedSet[E](left, right, _ && _, !_)
//}