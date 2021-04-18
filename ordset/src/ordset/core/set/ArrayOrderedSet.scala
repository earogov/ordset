package ordset.core.set

import ordset.array.SortedArraySearch
import ordset.core.domain.{Domain, DomainOps, OrderValidationFunc}
import ordset.core._
import ordset.random.RngManager

import scala.collection.immutable.ArraySeq

class ArrayOrderedSet[E, D <: Domain[E]] protected (
  final val bounds: ArraySeq[Bound.Upper[E]],
  final val complementary: Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractArraySegmentSeq[E, D, Boolean]
  with OrderedSetCommons[E, D] {

  import SortedArraySearch._

  validate()

  // Transformation ----------------------------------------------------------- //
  final override def appended(other: OrderedSet[E, D]): OrderedSet[E, D] = other match {
    case other: ArrayOrderedSet[E, D] => appendedArraySet(other)
    case _ => appendedSegmentSeq(other)
  }

  final override def appended(bound: Bound[E], other: OrderedSet[E, D]): OrderedSet[E, D] = {
    // original:
    //                bound  originalBoundSegment
    //                   )   /
    // X--false---](---true---](-----false----X
    //            0           1                   - bounds indexes
    //      0            1             2          - segments indexes
    //
    // other:
    //                      otherBoundsProvider.boundSegment
    //                       /
    // X--t--)[-------false------](---true----X
    //       0                   1                - bounds indexes
    //    0              1              2         - segments indexes
    //
    // original.appended(bound, other):
    //
    //                 bound
    //                   v
    // X--false---](--t--)[--f---](---true----X
    //            0      1       2                - bound indexes
    //      0         1      2          3         - segments indexes
    //
    val upperBound = bound.provideUpper
    val lowerBound = bound.provideLower

    val originalBoundSegment = getSegment(upperBound)
    
    val otherBoundsProvider = other match {
      case other: ArrayOrderedSet[E, D] => ArraySetBoundsProvider(other, lowerBound)
      case _ => GeneralBoundsProvider(other, lowerBound)
    }

    val originalBoundMatch = originalBoundSegment.hasUpperBound(upperBound)
    val boundValuesMatch = originalBoundSegment.hasValue(otherBoundsProvider.boundSegment.value)

    val originalCopyLen =
      if (originalBoundMatch && !boundValuesMatch) originalBoundSegment.index + 1
      else originalBoundSegment.index
    
    val otherCopyLen = otherBoundsProvider.copyLen

    if (originalCopyLen == bounds.length && otherCopyLen == 0 && boundValuesMatch) {
      this
    } else {
      val skipBound = originalBoundMatch || boundValuesMatch

      val newBoundsLen =
        if (skipBound) originalCopyLen + otherCopyLen
        else originalCopyLen + otherCopyLen + 1

      if (newBoundsLen == 0)
        consUniform(originalBoundSegment.value)
      else {
        val newBoundsArray = new Array[Bound.Upper[E]](newBoundsLen)
        if (originalCopyLen > 0) {
          Array.copy(bounds.unsafeArray, 0, newBoundsArray, 0, originalCopyLen)
        }
        var nextInd = originalCopyLen
        if (!skipBound) {
          newBoundsArray(originalCopyLen) = upperBound
          nextInd = nextInd + 1
        }
        otherBoundsProvider.copyToArray(newBoundsArray, nextInd)
        new ArrayOrderedSet[E, D](ArraySeq.unsafeWrapArray(newBoundsArray), complementary)
      }
    }
  }

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def getSegmentValue(ind: Int): Boolean = isIncludedInSet(ind)

  @inline
  protected final def consUniform(value: Boolean): UniformOrderedSet[E, D] = UniformOrderedSet(value)

  protected final def consAbove(ind: Int): ArrayOrderedSet[E, D] = {
    val newComplementary = getSegmentValue(ind)
    val len = bounds.length - ind
    val newBoundsArray = new Array[Bound.Upper[E]](len)
    Array.copy(bounds.unsafeArray, ind, newBoundsArray, 0, len)
    new ArrayOrderedSet(ArraySeq.unsafeWrapArray(newBoundsArray), newComplementary)
  }

  protected final def consBelow(ind: Int): ArrayOrderedSet[E, D] = {
    val len = ind + 1
    val newBoundsArray = new Array[Bound.Upper[E]](len)
    Array.copy(bounds.unsafeArray, 0, newBoundsArray, 0, len)
    new ArrayOrderedSet(ArraySeq.unsafeWrapArray(newBoundsArray), complementary)
  }

  // Private section ---------------------------------------------------------- //
  private def appendedArraySet(other: ArrayOrderedSet[E, D]): OrderedSet[E, D] = {
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
      if (valueOps.eqv(originalPenultimateValue, appendedFirstValue)) bounds.length - 1
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

  private def appendedSegmentSeq(other: OrderedSet[E, D]): OrderedSet[E, D] = {
    val originalPenultimateValue = getPenultimateSegmentValue
    if (other.isUniform)
      if (valueOps.eqv(originalPenultimateValue, other.firstSegment.value))
        if (bounds.length == 1) consUniform(complementary)
        else consBelow(lastBoundIndex - 1)
      else this
    else {
      val appendedFirstSegment = other.getSegment(bounds(lastBoundIndex).flip)

      val (otherCopyList, otherCopyLen) = SegmentSeqOps.getForwardBoundsList(appendedFirstSegment)

      val originalCopyLen =
        if (valueOps.eqv(originalPenultimateValue, appendedFirstSegment.value)) bounds.length - 1
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
  
  private sealed trait BoundsProvider() {

    val boundSegment: GenSegment
    
    def copyLen: Int
    
    def copyToArray(arr: Array[Bound.Upper[E]], start: Int): Unit
  }
  
  private case class ArraySetBoundsProvider(
    segmentSeq: ArrayOrderedSet[E, D],
    bound: Bound.Lower[E]
  ) extends BoundsProvider {
    
    override val boundSegment: AbstractIndexedSegmentSeq.IndexedSegmentBase[E, D, Boolean] with GenSegment = 
      segmentSeq.getSegment(bound)
    
    override val copyLen: Int = segmentSeq.bounds.length - boundSegment.index
    
    override def copyToArray(arr: Array[Bound.Upper[E]], start: Int): Unit =
      if (copyLen > 0) Array.copy(segmentSeq.bounds.unsafeArray, boundSegment.index, arr, start, copyLen)
  }
  
  private case class GeneralBoundsProvider(
    segmentSeq: OrderedSet[E, D],
    bound: Bound.Lower[E]
  ) extends BoundsProvider {

    override val boundSegment: GenSegment = segmentSeq.getSegment(bound)

    private val copyBounds: (List[Bound.Upper[E]], Int) = SegmentSeqOps.getForwardBoundsList(boundSegment)

    override def copyLen: Int = copyBounds._2

    override def copyToArray(arr: Array[Bound.Upper[E]], start: Int): Unit =
      if (copyLen > 0) copyBounds._1.copyToArray(arr, start, copyLen)
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