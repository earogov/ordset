package ordset.util

import ordset.Order

import scala.Specializable.{AllNumeric => spNum}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.{specialized => sp}

object SortedArraySearch {

  val NotFound: Int = -1

  @throws(classOf[IllegalArgumentException])
  @throws(classOf[ArrayIndexOutOfBoundsException])
  def binSearchClosestNotGreater[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]): Int = {

    requireValidIndexes(start, end, array.length)
    uncheckedBinSearchClosestNotGreater(element, order, array, start, end)
  }

  @throws(classOf[IllegalArgumentException])
  @throws(classOf[ArrayIndexOutOfBoundsException])
  def binSearchClosestNotLess[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]): Int = {

    requireValidIndexes(start, end, array.length)
    uncheckedBinSearchClosestNotLess(element, order, array, start, end)
  }

  @throws(classOf[IllegalArgumentException])
  @throws(classOf[ArrayIndexOutOfBoundsException])
  def linSearchClosestNotGreater[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]): Int = {

    requireValidIndexes(start, end, array.length)
    uncheckedLinSearchClosestNotGreater(element, order, array, start, end)
  }

  @throws(classOf[IllegalArgumentException])
  @throws(classOf[ArrayIndexOutOfBoundsException])
  def linSearchClosestNotLess[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]): Int = {

    requireValidIndexes(start, end, array.length)
    uncheckedLinSearchClosestNotLess(element, order, array, start, end)
  }

  @throws(classOf[IllegalArgumentException])
  @throws(classOf[ArrayIndexOutOfBoundsException])
  def optimisticBinSearchClosestNotGreater[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]): Int = {

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

  @throws(classOf[IllegalArgumentException])
  @throws(classOf[ArrayIndexOutOfBoundsException])
  def optimisticBinSearchClosestNotLess[@sp(spNum) E](
      element: E,
      array: ArraySeq[E]
  )(
      start: Int = 0,
      end: Int = array.length - 1
  )(
      implicit order: Order[E]): Int = {

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

  @throws(classOf[ArrayIndexOutOfBoundsException])
  private def uncheckedBinSearchClosestNotGreater[@sp(spNum) E](
      element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int): Int = {

    @tailrec
    @throws(classOf[ArrayIndexOutOfBoundsException])
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

  @throws(classOf[ArrayIndexOutOfBoundsException])
  private def uncheckedBinSearchClosestNotLess[@sp(spNum) E](
      element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int): Int = {

    @tailrec
    @throws(classOf[ArrayIndexOutOfBoundsException])
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

  @throws(classOf[ArrayIndexOutOfBoundsException])
  private def uncheckedLinSearchClosestNotGreater[@sp(spNum) E](
      element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int): Int = {

    @tailrec
    @throws(classOf[ArrayIndexOutOfBoundsException])
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

  @throws(classOf[ArrayIndexOutOfBoundsException])
  private def uncheckedLinSearchClosestNotLess[@sp(spNum) E](
      element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int): Int = {

    @tailrec
    @throws(classOf[ArrayIndexOutOfBoundsException])
    def loop(element: E, order: Order[E], array: ArraySeq[E], start: Int, end: Int): Int = {
      val cmp = order.compare(array(start), element)
      if (cmp >= 0) start
      else if (start == end) NotFound
      else loop(element, order, array, start + 1, end)
    }

    loop(element, order, array, start, end)
  }

  @inline
  @throws(classOf[IllegalArgumentException])
  @throws(classOf[ArrayIndexOutOfBoundsException])
  private def requireValidIndexes(start: Int, end: Int, length: Int): Unit = {
    if (start < 0 || start >= length) throw new ArrayIndexOutOfBoundsException(start)
    else if (end < 0 || end >= length) throw new ArrayIndexOutOfBoundsException(end)
    else if (start > end) throw new IllegalArgumentException(s"start index ($start) > end index ($end)")
  }
}
