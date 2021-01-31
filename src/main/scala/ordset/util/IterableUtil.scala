package ordset.util

import ordset.{Eq, Order}

object IterableUtil {

  def iteratorCompare[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Order[A]): Int =
    cats.kernel.instances.StaticMethods.iteratorCompare(xs, ys)(ev)

  def iteratorEq[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Eq[A]): Boolean =
    cats.kernel.instances.StaticMethods.iteratorEq(xs, ys)(ev)

  /**
   * Returns next element if iterator. If there is no such element throws exception with specified message.
   */
  @throws[NoSuchElementException]
  def nextOrThrowMsg[A](i: Iterator[A], msg: String): A = {
    if (i.hasNext) i.next()
    else throw new NoSuchElementException(msg)
  }
}
