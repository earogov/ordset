package ordset.core

import ordset.core.domain.Domain

import scala.collection.immutable.ArraySeq

/**
 * For common description of segment sequence see [[SegmentSeq]].
 *
 * <u>Class is not intended to model empty and universal sets.</u>
 * For such cases implementation based on [[UniformSegmentSeq]] can be used.
 *
 * Upper bounds of segments are stored in `bounds` array based collection.
 *
 * <tr>`bounds` collection must be non-empty. </tr>
 */
abstract class AbstractArraySegmentSeq[E, D[X] <: Domain[X], V] extends AbstractIndexedSegmentSeq[E, D, V] {

  import ordset.array.SortedArraySearch._

  // Inspection --------------------------------------------------------------- //
  override val bounds: ArraySeq[Bound.Upper[E]]
  
  // Protected section -------------------------------------------------------- //
  protected override def searchSegmentFromBegin(bound: Bound[E]): Int = {
    val res = binSearchClosestNotLess[Bound[E]](
      bound, bounds
    )(
      0, lastBoundIndex
    )(
      domainOps.boundOrd
    )
    // Target element is greater then all elements in array.
    if (res == NotFound) lastSegmentIndex
    // Accept result of binary search.
    else res
  }

  protected override def searchSegmentFromIndex(ind: Int, bound: Bound[E]): Int = {
    val limitedInd = if (ind == lastSegmentIndex) lastBoundIndex else ind
    // Search function is optimized for the case of sequential traverse through segments with some jumps forward
    // to skip some of them. At first it uses a variation of binary search to look forward from the current index.
    val res = optimisticBinSearchClosestNotLess[Bound[E]](
      bound, bounds
    )(
      limitedInd, lastBoundIndex
    )(
      domainOps.boundOrd
    )
    // Target element is greater then all elements in array.
    if (res == NotFound) lastSegmentIndex
    // We have got first index. It's possible that there is better solution (i.e. closer to target `bound`) backwards.
    else if (res == limitedInd) binSearchClosestNotLess[Bound[E]](
      bound, bounds
    )(
      0, limitedInd
    )(
      domainOps.boundOrd
    )
    // Accept result of binary search.
    else res
  }
}

// TODO implement segments to override type of indexed sequence.
object AbstractArraySegmentSeq {
}
