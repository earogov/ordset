package ordset.util

import ordset.{Eq, Order}

object IterableUtil {

  def iteratorCompare[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Order[A]): Int =
    cats.kernel.instances.StaticMethods.iteratorCompare(xs, ys)(ev)

  def iteratorEq[A](xs: Iterator[A], ys: Iterator[A])(implicit ev: Eq[A]): Boolean =
    cats.kernel.instances.StaticMethods.iteratorEq(xs, ys)(ev)
}
