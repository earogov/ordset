package ordset.array

import ordset.Order

import scala.Specializable.{AllNumeric => spNum}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.{specialized => sp}

/**
 * Utility methods for search in sorted arrays.
 *
 * Input array must be sorted according to specified order (strictly):
 * {{{
 *   order.compare(array[i], array[i-1]) > 0 for i in [1, array.length - 1]
 * }}}
 */
object SortedArraySearch {

  /**
   * Output value when element is not found.
   */
  val NotFound: Int = -1

  /**
   * Binary search of of closest element not greater then specified.
   * @param element required element
   * @param array sorted array
   * @param start start index (including)
   * @param end end index (including)
   * @param order order typeclass
   */
  @throws[IllegalArgumentException]
  @throws[ArrayIndexOutOfBoundsException]
  def binSearchClosestNotGreater[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]
  ): Int = {

    requireValidIndexes(start, end, array.length)
    uncheckedBinSearchClosestNotGreater(element, order, array, start, end)
  }

  /**
   * Binary search of of closest element less then specified.
   * @param element required element
   * @param array sorted array
   * @param start start index (including)
   * @param end end index (including)
   * @param order order typeclass
   */
  @throws[IllegalArgumentException]
  @throws[ArrayIndexOutOfBoundsException]
  def binSearchClosestLess[@sp(spNum) E](
    element: E,
    array: ArraySeq[E]
  )(
    start: Int = 0,
    end: Int = array.length - 1
  )(
    implicit order: Order[E]
  ): Int = {

    requireValidIndexes(start, end, array.length)
    uncheckedBinSearchClosestLess(element, order, array, start, end)
  }

  /**
   * Binary search of of closest element not less then specified.
   * @param element required element
   * @param array sorted array
   * @param start start index (including)
   * @param end end index (including)
   * @param order order typeclass
   */
  @throws[IllegalArgumentException]
  @throws[ArrayIndexOutOfBoundsException]
  def binSearchClosestNotLess[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]
  ): Int = {

    requireValidIndexes(start, end, array.length)
    uncheckedBinSearchClosestNotLess(element, order, array, start, end)
  }

  /**
   * Binary search of of closest element greater then specified.
   * @param element required element
   * @param array sorted array
   * @param start start index (including)
   * @param end end index (including)
   * @param order order typeclass
   */
  @throws[IllegalArgumentException]
  @throws[ArrayIndexOutOfBoundsException]
  def binSearchClosestGreater[@sp(spNum) E](
    element: E,
    array: ArraySeq[E]
  )(
    start: Int = 0,
    end: Int = array.length - 1
  )(
    implicit order: Order[E]
  ): Int = {

    requireValidIndexes(start, end, array.length)
    uncheckedBinSearchClosestGreater(element, order, array, start, end)
  }

  /**
   * Linear search of of closest element not greater then specified.
   * Note O(n) performance.
   * @param element required element
   * @param array sorted array
   * @param start start index (including)
   * @param end end index (including)
   * @param order order typeclass
   */
  @throws[IllegalArgumentException]
  @throws[ArrayIndexOutOfBoundsException]
  def linSearchClosestNotGreater[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]
  ): Int = {

    requireValidIndexes(start, end, array.length)
    uncheckedLinSearchClosestNotGreater(element, order, array, start, end)
  }

  /**
   * Linear search of of closest element not less then specified.
   * Note O(n) performance.
   * @param element required element
   * @param array sorted array
   * @param start start index (including)
   * @param end end index (including)
   * @param order order typeclass
   */
  @throws[IllegalArgumentException]
  @throws[ArrayIndexOutOfBoundsException]
  def linSearchClosestNotLess[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]
  ): Int = {

    requireValidIndexes(start, end, array.length)
    uncheckedLinSearchClosestNotLess(element, order, array, start, end)
  }

  /**
   * Binary + linear search of closest element not greater then specified.
   * Search optimized for cases when required element is located with high probability closer to start index.
   * Algorithm makes few steps of linear search and goes to binary search if it fails.
   * @param element required element
   * @param array sorted array
   * @param start start index (including)
   * @param end end index (including)
   * @param order order typeclass
   */
  @throws[IllegalArgumentException]
  @throws[ArrayIndexOutOfBoundsException]
  def optimisticBinSearchClosestNotGreater[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]
  ): Int = {

    @inline
    def seqSteps(length: Int): Int = (Integer.SIZE - Integer. numberOfLeadingZeros(length)) >>> 1

    requireValidIndexes(start, end, array.length)
    val len = end - start
    if (len <= 8) uncheckedLinSearchClosestNotGreater(element, order, array, start, end)
    else {
      val linEnd = start + seqSteps(len)
      if (order.compare(array(linEnd), element) >= 0)
        uncheckedLinSearchClosestNotGreater(element, order, array, start, linEnd)
      else
        uncheckedBinSearchClosestNotGreater(element, order, array, linEnd, end)
    }
  }

  /**
   * Binary + linear search of closest element not less then specified.
   * Search optimized for cases when required element is located with high probability closer to start index.
   * Algorithm makes few steps of linear search and goes to binary search if it fails.
   * @param element required element
   * @param array sorted array
   * @param start start index (including)
   * @param end end index (including)
   * @param order order typeclass
   */
  @throws[IllegalArgumentException]
  @throws[ArrayIndexOutOfBoundsException]
  def optimisticBinSearchClosestNotLess[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]
  ): Int = {

    @inline
    def seqSteps(length: Int): Int = (Integer.SIZE - Integer. numberOfLeadingZeros(length)) >>> 1

    requireValidIndexes(start, end, array.length)
    val len = end - start
    if (len <= 8) uncheckedLinSearchClosestNotLess(element, order, array, start, end)
    else {
      val linEnd = start + seqSteps(len)
      if (order.compare(array(linEnd), element) >= 0)
        uncheckedLinSearchClosestNotLess(element, order, array, start, linEnd)
      else
        uncheckedBinSearchClosestNotLess(element, order, array, linEnd, end)
    }
  }

  // Private section ---------------------------------------------------------- //
  @throws[ArrayIndexOutOfBoundsException]
  private def uncheckedBinSearchClosestNotGreater[@sp(spNum) E](
      element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int
  ): Int = {

    @tailrec
    @throws[ArrayIndexOutOfBoundsException]
    def loop (element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int): Int = {
      val len = end - start
      if (len == 1)
        if (order.compare(array(start), element) > 0) NotFound
        else if (order.compare(array(end), element) > 0) start
        else end
      else {
        val mid = start + (len >> 1)
        val cmp = order.compare(array(mid), element)
        if (cmp > 0) loop(element, order, array, start, mid)
        else if (cmp < 0) loop(element, order, array, mid, end)
        else mid
      }
    }

    if (array.length == 1 || start == end)
      if (order.compare(array(start), element) > 0) NotFound
      else start
    else loop(element, order, array, start, end)
  }

  @throws[ArrayIndexOutOfBoundsException]
  private def uncheckedBinSearchClosestLess[@sp(spNum) E](
    element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int
  ): Int = {

    @tailrec
    @throws[ArrayIndexOutOfBoundsException]
    def loop (element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int): Int = {
      val len = end - start
      if (len == 1)
        if (order.compare(array(start), element) >= 0) NotFound
        else if (order.compare(array(end), element) >= 0) start
        else end
      else {
        val mid = start + (len >> 1)
        val cmp = order.compare(array(mid), element)
        if (cmp >= 0) loop(element, order, array, start, mid)
        else loop(element, order, array, mid, end)
      }
    }

    if (array.length == 1 || start == end)
      if (order.compare(array(start), element) >= 0) NotFound
      else start
    else loop(element, order, array, start, end)
  }

  @throws[ArrayIndexOutOfBoundsException]
  private def uncheckedBinSearchClosestNotLess[@sp(spNum) E](
      element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int
  ): Int = {

    @tailrec
    @throws[ArrayIndexOutOfBoundsException]
    def loop (element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int): Int = {
      val len = end - start
      if (len == 1)
        if (order.compare(array(end), element) < 0) NotFound
        else if (order.compare(array(start), element) < 0) end
        else start
      else {
        val mid = start + (len >> 1)
        val cmp = order.compare(array(mid), element)
        if (cmp > 0) loop(element, order, array, start, mid)
        else if (cmp < 0) loop(element, order, array, mid, end)
        else mid
      }
    }

    if (array.length == 1 || start == end)
      if (order.compare(array(start), element) < 0) NotFound
      else start
    else loop(element, order, array, start, end)
  }

  @throws[ArrayIndexOutOfBoundsException]
  private def uncheckedBinSearchClosestGreater[@sp(spNum) E](
    element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int
  ): Int = {

    @tailrec
    @throws[ArrayIndexOutOfBoundsException]
    def loop (element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int): Int = {
      val len = end - start
      if (len == 1)
        if (order.compare(array(end), element) <= 0) NotFound
        else if (order.compare(array(start), element) <= 0) end
        else start
      else {
        val mid = start + (len >> 1)
        val cmp = order.compare(array(mid), element)
        if (cmp > 0) loop(element, order, array, start, mid)
        else loop(element, order, array, mid, end)
      }
    }

    if (array.length == 1 || start == end)
      if (order.compare(array(start), element) <= 0) NotFound
      else start
    else loop(element, order, array, start, end)
  }

  @throws[ArrayIndexOutOfBoundsException]
  private def uncheckedLinSearchClosestNotGreater[@sp(spNum) E](
      element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int
  ): Int = {

    @tailrec
    @throws[ArrayIndexOutOfBoundsException]
    def loop(element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int): Int = {
      val cmp = order.compare(array(start), element)
      if (cmp > 0) start - 1
      else if (cmp == 0 || start == end ) start
      else loop(element, order, array, start + 1, end)
    }

    val cmp = order.compare(array(start), element)
    if (cmp > 0) NotFound
    else if (cmp == 0 || start == end ) start
    else loop(element, order, array, start + 1, end)
  }

  @throws[ArrayIndexOutOfBoundsException]
  private def uncheckedLinSearchClosestNotLess[@sp(spNum) E](
      element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int
  ): Int = {

    @tailrec
    @throws[ArrayIndexOutOfBoundsException]
    def loop(element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int): Int = {
      val cmp = order.compare(array(start), element)
      if (cmp >= 0) start
      else if (start == end) NotFound
      else loop(element, order, array, start + 1, end)
    }

    loop(element, order, array, start, end)
  }

  @inline
  @throws[IllegalArgumentException]
  @throws[ArrayIndexOutOfBoundsException]
  private final def requireValidIndexes(start: Int, end: Int, length: Int): Unit = {
    if (start < 0 || start >= length) throw new ArrayIndexOutOfBoundsException(start)
    else if (end < 0 || end >= length) throw new ArrayIndexOutOfBoundsException(end)
    else if (start > end) throw new IllegalArgumentException(s"start index ($start) > end index ($end)")
  }
}
