package ordset

import scala.collection.immutable.ArraySeq

sealed class ArrayOrdset[T] (
  protected val bounds: ArraySeq[Bound.Upper[T]],
  protected val complement: Boolean
)(
  implicit
  override val order: Order[Bound[T]],
) extends OrderedSet[T] {

  import OrderedSet._
  import SortedArraySearch._

  override def isEmpty: Boolean = bounds.isEmpty && !complement

  override def isUniversal: Boolean = bounds.isEmpty && complement

  override def intervalFlow: IFlow[T] = {

    /** @return interval flow for index `ind`. */
    def makeFlow(ind: Int): IFlow[T] =
      if (bounds.isEmpty)
        if (ind <= 0) IntervalFlow.Active(moveToForEmpty, moveNext(ind))
        else IntervalFlow.Finished(moveToForEmpty)
      else
        if (ind > 0 && ind < bounds.length) IntervalFlow.Active(moveToFromIndex(ind), moveNext(ind))
        else if (ind <= 0 || ind == bounds.length) IntervalFlow.Active(moveToFromBegin, moveNext(ind))
        else IntervalFlow.Finished(moveToFromBegin)

    /** @return function that allows to get next interval from index `ind`. */
    def moveNext(ind: Int): () => IFlowResult[T] = () => (makeFlow(ind + 1), getIntervalMapping(ind))

    /**
      * @param ind - current index of interval flow.
      * @return function that allows to get interval for specified bound `b`.
      * @note preconditions:
      *       1. `bounds` array must be non empty.
      *       2. `ind` must be between 0 and `bounds.length` - 1.
      */
    def moveToFromIndex(ind: Int): Bound[T] => IFlowResult[T] = b => {
      // The most probable use case of flow is sequential traverse through elements with some jumps forward to skip
      // some of them. So first of all we search forward from current index using a variation of binary search.
      // It picks a small chunk from start for linear search and uses binary search for the rest.
      val res = optimisticBinSearchClosestNotLess[Bound[T]](b, bounds)(ind, lastIndex)(order)
      val newInd =
        // Target element is greater then all elements in array.
        if (res == NotFound) bounds.length
        // We'v got first index. It's possible that there is better solution (i.e. closer to target) backwards.
        else if (res == ind) binSearchClosestNotLess[Bound[T]](b, bounds)(0, ind)(order)
        // Accept result of binary search.
        else res
      (makeFlow(newInd), getIntervalMapping(newInd))
    }

    /**
      * @return function that allows to get interval for specified bound `b`.
      * @note preconditions:
      *       1. `bounds` array must be non empty.
      */
    def moveToFromBegin: Bound[T] => IFlowResult[T] = b => {
      val res = binSearchClosestNotLess[Bound[T]](b, bounds)(0, lastIndex)(order)
      val newInd =
        // Target element is greater then all elements in array.
        if (res == NotFound) bounds.length
        // Accept result of binary search.
        else res
      (makeFlow(newInd), getIntervalMapping(newInd))
    }

    /** @return function that allows to get the only interval in case of empty/universal ordered set. */
    def moveToForEmpty: Bound[T] => IFlowResult[T] = _ =>
      (makeFlow(0), getIntervalMapping(0))

    makeFlow(0)
  }

  protected def getIntervalMapping(ind: Int): IMapping[T] =
    IntervalMapping(getInterval(ind), belongsToSet(ind))

  @inline
  protected final def getInterval(ind: Int): Interval[T] =
    if      (bounds.isEmpty)       Interval.Unbounded
    else if (ind <= 0)             Interval.LeftUnbounded(getUpper(0))
    else if (ind >= bounds.length) Interval.RightUnbounded(getLower(lastIndex))
    else                           Interval.Bounded(getLower(ind - 1), getUpper(ind))

  @inline
  @throws(classOf[ArrayIndexOutOfBoundsException])
  protected final def getLower(ind: Int): Bound.Lower[T] = bounds(ind).flipUpper

  @inline
  @throws(classOf[ArrayIndexOutOfBoundsException])
  protected final def getUpper(ind: Int): Bound.Upper[T] = bounds(ind)

  @inline
  protected final def belongsToSet(ind: Int): Boolean = complement ^ ((ind & 0x00000001) == 0x00000001)

  @inline
  protected final def lastIndex: Int = bounds.length - 1
}

//object ArrayOrdset {
//
//  def combine[T, B <: OrdsetBuffer[T]](sets: Iterable[ArrayOrdset[T]], buffer: B): B = ???
//}
