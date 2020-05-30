package ordset

abstract class ArraySegmentSeq[E, W] extends AbstractSegmentSeq[E, W] {
  import SortedArraySearch._
  import domain._

  override protected val bounds: collection.immutable.ArraySeq[Bound.Upper[E]]

  override def searchSegmentFromBegin(bound: Bound[E]): Int = {
    val res = binSearchClosestNotLess[Bound[E]](bound, bounds)(0, lastBoundIndex)(boundOrd)
    // Target element is greater then all elements in array.
    if (res == NotFound) lastSegmentIndex
    // Accept result of binary search.
    else res
  }

  override def searchSegmentFromIndex(ind: Int, bound: Bound[E]): Int = {
    val limitedInd = if (ind == lastSegmentIndex) lastBoundIndex else ind
    // Search function is optimized for the case of sequential traverse through segments with some jumps forward
    // to skip some of them. At first it uses a variation of binary search to look forward from the current index.
    val res = optimisticBinSearchClosestNotLess[Bound[E]](bound, bounds)(limitedInd, lastBoundIndex)(boundOrd)
    // Target element is greater then all elements in array.
    if (res == NotFound) lastSegmentIndex
    // We'v got first index. It's possible that there is better solution (i.e. closer to target `bound`) backwards.
    else if (res == limitedInd) binSearchClosestNotLess[Bound[E]](bound, bounds)(0, limitedInd)(boundOrd)
    // Accept result of binary search.
    else res
  }
}
