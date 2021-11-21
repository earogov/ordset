package ordset

import cats.kernel.Comparison

object OrderComponents {

  trait OrderProxy[E] extends Order[E] {

    protected val original: Order[E]

    override def compare(x: E, y: E): Int = original.compare(x, y)

    override def comparison(x: E, y: E): Comparison = original.comparison(x, y)

    override def partialCompare(x: E, y: E): Double = original.partialCompare(x, y)

    override def min(x: E, y: E): E = original.min(x, y)

    override def max(x: E, y: E): E = original.max(x, y)

    override def eqv(x: E, y: E): Boolean = original.eqv(x, y)

    override def neqv(x: E, y: E): Boolean = original.neqv(x, y)

    override def lteqv(x: E, y: E): Boolean = original.lteqv(x, y)

    override def lt(x: E, y: E): Boolean = original.lt(x, y)

    override def gteqv(x: E, y: E): Boolean = original.gteqv(x, y)

    override def gt(x: E, y: E): Boolean = original.gt(x, y)

    override def toOrdering: Ordering[E] = original.toOrdering
  }

  object Reversed {

    trait OrderProxy[E] extends Order[E] {

      protected val original: Order[E]

      override def compare(x: E, y: E): Int = original.compare(y, x)

      override def comparison(x: E, y: E): Comparison = original.comparison(y, x)

      override def partialCompare(x: E, y: E): Double = original.partialCompare(y, x)

      override def min(x: E, y: E): E = original.min(y, x)

      override def max(x: E, y: E): E = original.max(y, x)

      override def eqv(x: E, y: E): Boolean = original.eqv(y, x)

      override def neqv(x: E, y: E): Boolean = original.neqv(y, x)

      override def lteqv(x: E, y: E): Boolean = original.lteqv(y, x)

      override def lt(x: E, y: E): Boolean = original.lt(y, x)

      override def gteqv(x: E, y: E): Boolean = original.gteqv(y, x)

      override def gt(x: E, y: E): Boolean = original.gt(y, x)

      override def toOrdering: Ordering[E] = original.toOrdering
    }
  }

  class IntDiscreteBoundedOrder()
    extends OrderProxy[Int] 
      with HashComponents.HashProxy[Int]
      with DiscreteComponents.Numeric.Discrete[Int]
      with BoundedComponents.IntBounded
      with Directed.Ascending {

    override val original: Order[Int] with Hash[Int] = cats.kernel.instances.int.catsKernelStdOrderForInt

    override val num: Numeric[Int] = implicitly[Numeric[Int]]
  }
}