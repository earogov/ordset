package ordset.core

import ordset.array.SortedArraySearch
import ordset.core.domain.{Domain, DomainOps, OrderValidationFunc}
import ordset.random.RngManager

import scala.collection.immutable.ArraySeq

class ArrayOrderedSet[E, D <: Domain[E]] protected (
  final val bounds: ArraySeq[Bound.Upper[E]],
  final val complementary: Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractArraySegmentSeq[E, D, Boolean] {

  import SortedArraySearch._

  validate()

  // Transformation ----------------------------------------------------------- //
  final override def appended(other: SegmentSeq[E, D, Boolean]): SegmentSeq[E, D, Boolean] = other match {
    case other: ArrayOrderedSet[E, D] => appendedArraySet(other)
    case _ => appendedGeneral(other)
  }

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def getSegmentValue(ind: Int): Boolean = isIncludedInSet(ind)

  @inline
  protected final def consUniform(value: Boolean) :SegmentSeq[E, D, Boolean] = UniformOrderedSet(value)

  protected final def consAbove(ind: Int): SegmentSeq[E, D, Boolean] = {
    val newComplementary = getSegmentValue(ind)
    val len = bounds.length - ind
    val newBoundsArray = new Array[Bound.Upper[E]](len)
    Array.copy(bounds.unsafeArray, ind, newBoundsArray, 0, len)
    ArrayOrderedSet.unchecked(ArraySeq.unsafeWrapArray(newBoundsArray), newComplementary)
  }

  protected final def consBelow(ind: Int): SegmentSeq[E, D, Boolean] = {
    val len = ind + 1
    val newBoundsArray = new Array[Bound.Upper[E]](len)
    Array.copy(bounds.unsafeArray, 0, newBoundsArray, 0, len)
    ArrayOrderedSet.unchecked(ArraySeq.unsafeWrapArray(newBoundsArray), complementary)
  }

  // Private section ---------------------------------------------------------- //
  private def appendedArraySet(other: ArrayOrderedSet[E, D]): SegmentSeq[E, D, Boolean] = {
    // original:
    //
    //      originalPenultimateVal
    //           v
    // false    true      false
    // -----](-------](-------------X
    //               ^
    //        originalLastBound
    //
    // other:
    //
    //        appendedFirstVal
    //             v
    //           true        false
    // -------------------)[--------X
    //                    ^
    //   bound with otherStartInd
    //
    // If originalPenultimateVal == appendedFirstVal segment with originalLastBound must be dropped!
    //
    // original.appended(other):
    //
    // false      true       false
    // -----](------------)[--------X

    val originalLastBound = bounds(lastBoundIndex)
    val originalPenultimateValue = getPenultimateSegmentValue

    val otherStartInd = binSearchClosestGreater[Bound[E]](
      originalLastBound, other.bounds
    )(
      0, other.bounds.length - 1
    )(
      domainOps.boundOrd
    )
    val otherCopyLen =
      if (otherStartInd == NotFound) 0
      else other.bounds.length - otherStartInd

    val appendedFirstValue =
      if (otherStartInd == NotFound) other.getLastSegmentValue
      else other.getSegmentValue(otherStartInd)

    val originalCopyLen =
      if (originalPenultimateValue == appendedFirstValue) bounds.length - 1
      else bounds.length

    val newBoundsLen = originalCopyLen + otherCopyLen
    if (newBoundsLen == 0)
      consUniform(appendedFirstValue)
    else {
      val newBoundsArray = new Array[Bound.Upper[E]](newBoundsLen)
      if (originalCopyLen > 0) {
        Array.copy(bounds.unsafeArray, 0, newBoundsArray, 0, originalCopyLen)
      }
      if (otherCopyLen > 0) {
        Array.copy(other.bounds.unsafeArray, otherStartInd, newBoundsArray, originalCopyLen, otherCopyLen)
      }
      new ArrayOrderedSet[E, D](ArraySeq.unsafeWrapArray(newBoundsArray), complementary)
    }
  }

  private def appendedGeneral(other: SegmentSeq[E, D, Boolean]): SegmentSeq[E, D, Boolean] =
    if (other.isUniform)
      if (getLastSegmentValue == other.firstSegment.value) this
      else if (bounds.length == 1) consUniform(complementary)
      else consBelow(lastBoundIndex - 1)
    else {
      val originalPenultimateValue = getPenultimateSegmentValue

      val appendedFirstSegment = other.getSegment(bounds(lastBoundIndex).flip)

      val (otherCopyList, otherCopyLen) = SegmentSeqOps.getForwardBoundsList(appendedFirstSegment)

      val originalCopyLen =
        if (originalPenultimateValue == appendedFirstSegment.value) bounds.length - 1
        else bounds.length

      val newBoundsLen = originalCopyLen + otherCopyLen
      if (newBoundsLen == 0)
        consUniform(appendedFirstSegment.value)
      else {
        val newBoundsArray = new Array[Bound.Upper[E]](newBoundsLen)
        if (originalCopyLen > 0) {
          Array.copy(bounds.unsafeArray, 0, newBoundsArray, 0, originalCopyLen)
        }
        otherCopyList.copyToArray(newBoundsArray, originalCopyLen, otherCopyLen)
        ArrayOrderedSet.unchecked(ArraySeq.unsafeWrapArray(newBoundsArray), complementary)
      }
    }
}

object ArrayOrderedSet {

  /**
   * Creates ordered set from treap node (see [[AbstractIndexedSegmentSeq]]).
   *
   * Validation of key order is not applied.
   */
  def unchecked[E, D <: Domain[E]](
    bounds: ArraySeq[Bound.Upper[E]],
    complementary: Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): OrderedSet[E, D] =
    if (bounds.isEmpty) UniformOrderedSet(complementary)
    else new ArrayOrderedSet[E, D](bounds, complementary)

  /**
   * Creates ordered set from collection of upper bounds.
   *
   * Preconditions:
   *
   * 1. `bounds` collection is ordered according to `validationFunc`:
   *
   * validationFunc(bounds^i-1^, bounds^i^) == true for each i in [1, bounds.size]
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def fromIterableUnsafe[E, D <: Domain[E]](
    bounds: IterableOnce[Bound.Upper[E]],
    complementary: Boolean,
    domainOps: DomainOps[E, D]
  )(
    validationFunc: OrderValidationFunc[Bound.Upper[E]] = domainOps.boundOrd.strictValidationFunc
  )(
    implicit rngManager: RngManager
  ): OrderedSet[E, D] = bounds match {
    case bounds: ArraySeq[Bound.Upper[E]] =>
      OrderValidationFunc.validateIterable(bounds, validationFunc)
      unchecked(bounds, complementary)(domainOps, rngManager)
    case _ =>
      val boundsArraySeq =
        OrderValidationFunc.foldIterableAfter[Bound.Upper[E], ArraySeq[Bound.Upper[E]]](
          bounds,
          validationFunc,
          ArraySeq.empty[Bound.Upper[E]],
          (seq, bnd) => seq.appended(bnd)
        )
      unchecked(boundsArraySeq, complementary)(domainOps, rngManager)
  }

  /**
   * Returns ordered set factory.
   */
  def getFactory[E, D <: Domain[E]](
    domainOps: DomainOps[E, D]
  )(
    validationFunc: OrderValidationFunc[Bound.Upper[E]] =  domainOps.boundOrd.strictValidationFunc
  )(
    implicit rngManager: RngManager
  ): OrderedSetFactory[E, D] =
    (bounds, complementary) => fromIterableUnsafe[E, D](bounds, complementary, domainOps)(validationFunc)
}