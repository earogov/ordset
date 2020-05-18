package ordset

abstract class ArraySegmentSeq[E, W] extends AbstractSegmentSeq[E, W] {
  import SortedArraySearch._

  override protected val bounds: collection.immutable.ArraySeq[Bound.Upper[E]]

  override def searchSegmentFromBegin(bound: Bound[E]): Int = {
    val res = binSearchClosestNotLess[Bound[E]](bound, bounds)(0, lastBoundIndex)(order)
    // Target element is greater then all elements in array.
    if (res == NotFound) lastSegmentIndex
    // Accept result of binary search.
    else res
  }

  override def searchSegmentFromIndex(ind: Int, bound: Bound[E]): Int = {
    // The most probable use case is sequential traverse through segments with some jumps forward to skip
    // some of them. So first of all we search forward from current index using a variation of binary search.
    // It picks a small chunk from start for linear search and uses binary search for the rest.
    val res = optimisticBinSearchClosestNotLess[Bound[E]](bound, bounds)(ind, lastBoundIndex)(order)
    // Target element is greater then all elements in array.
    if (res == NotFound) lastSegmentIndex
    // We'v got first index. It's possible that there is better solution (i.e. closer to target) backwards.
    else if (res == ind) binSearchClosestNotLess[Bound[E]](bound, bounds)(0, ind)(order)
    // Accept result of binary search.
    else res
  }
}
