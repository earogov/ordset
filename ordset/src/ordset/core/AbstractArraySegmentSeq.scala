package ordset.core

import ordset.core.domain.Domain

import scala.collection.immutable.ArraySeq

/**
 * For common description of segment sequence see [[SegmentSeq]].
 *
 * <u>Class is not intended to model empty and universal sets.</u>
 * For such cases implementation based on [[AbstractUniformSegmentSeq]] can be used.
 *
 * Upper bounds of segments are stored in `bounds` array based collection.
 *
 * <tr>`bounds` collection MUST be non empty. </tr>
 */
abstract class AbstractArraySegmentSeq[E, D <: Domain[E], W] extends AbstractIndexedSegmentSeq[E, D, W] {

  import ordset.array.SortedArraySearch._

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): SegmentSeq[E, D, W] = {
    val ind = binSearchClosestNotLess(
      bound, bounds
    )(
      0, lastBoundIndex
    )(
      domainOps.boundOrd
    )
    // Drop whole current sequence and return last segment.
    if (ind == NotFound) consUniform(getLastSegmentValue)
    // Keep whole current sequence.
    else if (ind == 0) this
    // Copy sequence from index.
    else consAbove(ind)
  }

  final override def takenBelow(bound: Bound[E]): SegmentSeq[E, D, W] = {
    val ind = binSearchClosestLess(
      bound, bounds
    )(
      0, lastBoundIndex
    )(
      domainOps.boundOrd
    )
    // Drop whole current sequence and return first segment.
    if (ind == NotFound) consUniform(getFirstSegmentValue)
    // Keep whole current sequence.
    else if (ind == lastBoundIndex) this
    // Copy sequence from index.
    else consBelow(ind)
  }

  final override def sliced(bound: Bound[E]): (SegmentSeq[E, D, W], SegmentSeq[E, D, W]) = {
    val ind = binSearchClosestNotLess(
      bound, bounds
    )(
      0, lastBoundIndex
    )(
      domainOps.boundOrd
    )
    if (ind == NotFound) (this, consUniform(getLastSegmentValue))
    else if (ind == 0) (consUniform(getFirstSegmentValue), this)
    else (consBelow(ind - 1), consAbove(ind))
  }

  // Protected section -------------------------------------------------------- //
  protected override val bounds: ArraySeq[Bound.Upper[E]]

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
