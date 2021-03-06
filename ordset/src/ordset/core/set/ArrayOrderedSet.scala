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

  import AbstractIndexedSegmentSeq._
  import SortedArraySearch._

  validate()

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def getSegmentValue(ind: Int): Boolean = isIncludedInSet(ind)

  @inline
  protected final override def consUniform(value: Boolean): UniformOrderedSet[E, D] = UniformOrderedSet(value)

  protected final override def consAbove(ind: Int): ArrayOrderedSet[E, D] = {
    val newComplementary = getSegmentValue(ind)
    val len = bounds.length - ind
    val newBoundsArray = new Array[Bound.Upper[E]](len)
    Array.copy(bounds.unsafeArray, ind, newBoundsArray, 0, len)
    new ArrayOrderedSet(ArraySeq.unsafeWrapArray(newBoundsArray), newComplementary)
  }

  protected final override def consBelow(ind: Int): ArrayOrderedSet[E, D] = {
    val len = ind + 1
    val newBoundsArray = new Array[Bound.Upper[E]](len)
    Array.copy(bounds.unsafeArray, 0, newBoundsArray, 0, len)
    new ArrayOrderedSet(ArraySeq.unsafeWrapArray(newBoundsArray), complementary)
  }

  protected final override def appendedInternal(
    bound: Bound[E],
    segmentFunc: Bound.Upper[E] => IndexedSegment[E, D, Boolean],
    other: OrderedSet[E, D]
  ): IndexedSegmentSeq[E, D, Boolean] = {

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

    val originalBoundSegment = segmentFunc(upperBound)

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

  // Private section ---------------------------------------------------------- //
  private sealed trait BoundsProvider() {

    val boundSegment: Segment[E, D, Boolean]
    
    def copyLen: Int
    
    def copyToArray(arr: Array[Bound.Upper[E]], start: Int): Unit
  }
  
  private case class ArraySetBoundsProvider(
    segmentSeq: ArrayOrderedSet[E, D],
    bound: Bound.Lower[E]
  ) extends BoundsProvider {
    
    override val boundSegment: IndexedSegment[E, D, Boolean] =
      segmentSeq.getSegment(bound)
    
    override val copyLen: Int = segmentSeq.bounds.length - boundSegment.index
    
    override def copyToArray(arr: Array[Bound.Upper[E]], start: Int): Unit =
      if (copyLen > 0) Array.copy(segmentSeq.bounds.unsafeArray, boundSegment.index, arr, start, copyLen)
  }
  
  private case class GeneralBoundsProvider(
    segmentSeq: OrderedSet[E, D],
    bound: Bound.Lower[E]
  ) extends BoundsProvider {

    override val boundSegment: Segment[E, D, Boolean] = segmentSeq.getSegment(bound)

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