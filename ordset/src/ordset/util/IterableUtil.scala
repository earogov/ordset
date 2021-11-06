package ordset.util

import ordset.{Eq, Order}

protected[ordset] object IterableUtil {

  def iterableCompare[A](xs: Iterable[A], ys: Iterable[A])(implicit ev: Order[A]): Int =
    iteratorCompare(xs.iterator, ys.iterator)
  
  def iteratorCompare[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Order[A]): Int =
    cats.kernel.instances.StaticMethods.iteratorCompare(xs, ys)(ev)

  def iterableEq[A](xs: Iterable[A], ys: Iterable[A])(implicit ev: Eq[A]): Boolean =
    iteratorEq(xs.iterator, ys.iterator)
  
  def iteratorEq[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Eq[A]): Boolean =
    cats.kernel.instances.StaticMethods.iteratorEq(xs, ys)(ev)
}
