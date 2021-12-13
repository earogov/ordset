package ordset

import cats.kernel.Comparison
import scala.collection.immutable.Queue

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

  class UnitDiscreteFiniteOrder()
    extends cats.kernel.instances.UnitOrder
      with DiscreteOrder.Finite[Unit, Unit, Unit]
      with DiscreteComponents.Unit.Discrete
      with BoundedComponents.UnitBounded
      with Directed.Ascending

  class BooleanDiscreteFiniteOrder()
    extends cats.kernel.instances.BooleanOrder
    with DiscreteOrder.Finite[Boolean, Boolean, Boolean]
    with DiscreteComponents.Boolean.Discrete
    with BoundedComponents.BooleanBounded
    with Directed.Ascending

  class ByteDiscreteFiniteOrder()
    extends cats.kernel.instances.ByteOrder
    with DiscreteOrder.Finite[Byte, Byte, Byte]
    with DiscreteComponents.Numeric.Discrete[Byte]
    with BoundedComponents.ByteBounded
    with Directed.Ascending {

    override protected val num: Numeric[Byte] = implicitly[Numeric[Byte]]
  }

  class ShortDiscreteFiniteOrder()
    extends cats.kernel.instances.ShortOrder
    with DiscreteOrder.Finite[Short, Short, Short]
    with DiscreteComponents.Numeric.Discrete[Short]
    with BoundedComponents.ShortBounded
    with Directed.Ascending {

    override protected val num: Numeric[Short] = implicitly[Numeric[Short]]
  }

  class IntDiscreteFiniteOrder()
    extends cats.kernel.instances.IntOrder
    with DiscreteOrder.Finite[Int, Int, Int]
    with DiscreteComponents.Numeric.Discrete[Int]
    with BoundedComponents.IntBounded
    with Directed.Ascending {

    override protected val num: Numeric[Int] = implicitly[Numeric[Int]]
  }

  class LongDiscreteFiniteOrder()
    extends cats.kernel.instances.LongOrder
    with DiscreteOrder.Finite[Long, Long, Long]
    with DiscreteComponents.Numeric.Discrete[Long]
    with BoundedComponents.LongBounded
    with Directed.Ascending {

    override protected val num: Numeric[Long] = implicitly[Numeric[Long]]
  }

  class FloatBoundedOrder()
    extends cats.kernel.instances.FloatOrder
    with BoundedOrder.Including[Float, Float, Float]
    with BoundedComponents.FloatBounded
    with Directed.Ascending

  class DoubleBoundedOrder()
    extends cats.kernel.instances.DoubleOrder
    with BoundedOrder.Including[Double, Double, Double]
    with BoundedComponents.DoubleBounded
    with Directed.Ascending

  class StringBoundedBelowOrder()
    extends cats.kernel.instances.StringOrder
    with BoundedOrder.Below.Including[String, String]
    with BoundedComponents.StringBoundedBelow
    with Directed.Ascending

  class ListBoundedBelowOrder[T](
    implicit 
    ord: Order[T],
    hash: Hash[T]
  ) extends cats.kernel.instances.ListOrder[T]
    with Hash[List[T]]
    with BoundedOrder.Below.Including[List[T], Nil.type]
    with BoundedComponents.ListBoundedBelow
    with Directed.Ascending {

    override def hash(x: List[T]): Int = cats.kernel.instances.StaticMethods.listHash(x)
  }

  class QueueBoundedBelowOrder[T](
    implicit 
    ord: Order[T],
    hash: Hash[T]
  ) extends cats.kernel.instances.QueueOrder[T]
    with Hash[Queue[T]]
    with BoundedOrder.Below.Including[Queue[T], Queue[Nothing]]
    with BoundedComponents.QueueBoundedBelow
    with Directed.Ascending {

    override def hash(x: Queue[T]): Int = cats.kernel.instances.StaticMethods.orderedHash(x)
  }

  class LazyListBoundedBelowOrder[T](
    implicit 
    ord: Order[T],
    hash: Hash[T]
  ) extends cats.kernel.instances.LazyListOrder[T]
    with Hash[LazyList[T]]
    with BoundedOrder.Below.Including[LazyList[T], LazyList[Nothing]]
    with BoundedComponents.LazyListBoundedBelow
    with Directed.Ascending {

    override def hash(x: LazyList[T]): Int = cats.kernel.instances.StaticMethods.orderedHash(x)
  }
}