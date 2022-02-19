package ordset

import ordset.BoundedOrder.validateBounds

import scala.collection.immutable.Queue
import scala.util.Try

object implementations {
  
  object unit {

    trait NaturalBounds extends Bounded.Including[Unit, Unit] {

      override val lowerBound: Unit = {}

      override val upperBound: Unit = {}
    }

    trait Succeeding extends Discrete.Succeeding[Unit] {

      override def successorOrNone(x: Unit): Discrete.None.type = Discrete.None

      override def successorOpt(x: Unit): None.type = None
    }

    trait Preceding extends Discrete.Preceding[Unit] {

      override def predecessorOrNone(x: Unit): Discrete.None.type = Discrete.None

      override def predecessorOpt(x: Unit): None.type = None
    }

    trait Discrete extends ordset.Discrete[Unit] with Succeeding with Preceding

    class NaturalOrder()
      extends cats.kernel.instances.UnitOrder
      with DiscreteOrder.Finite[Unit, Unit, Unit]
      with Discrete
      with NaturalBounds {

      override val reversed: DiscreteOrder.Finite[Unit, Unit, Unit] = this
    }
  }

  object boolean {

    trait NaturalBounds extends Bounded.Including[Boolean, Boolean] {

      override val lowerBound: Boolean = false

      override val upperBound: Boolean = true
    }

    trait Succeeding extends Discrete.Succeeding[Boolean] {

      override def successorOrNone(x: Boolean): Discrete.Maybe[Boolean] = 
        if !x then true else Discrete.None

      override def successorOpt(x: Boolean): Option[Boolean] =
        if !x then Some(true) else Option.empty
    }

    trait Preceding extends Discrete.Preceding[Boolean] {

      override def predecessorOrNone(x: Boolean): Discrete.Maybe[Boolean] = 
        if x then false else Discrete.None

      override def predecessorOpt(x: Boolean): Option[Boolean] =
        if x then Some(false) else Option.empty
    }

    trait Discrete extends ordset.Discrete[Boolean] with Succeeding with Preceding
  
    class NaturalOrder()
      extends cats.kernel.instances.BooleanOrder
      with DiscreteOrder.Finite[Boolean, Boolean, Boolean]
      with Discrete
      with NaturalBounds
  }

  object numeric {

    trait Succeeding[E] extends Discrete.Succeeding[E] {

      protected def num: Numeric[E]

      override def successorOrNone(x: E): Discrete.Maybe[E] = 
        if hasSuccessor(x) then num.plus(x, num.one) else Discrete.None

      override def successorOpt(x: E): Option[E] = 
        if hasSuccessor(x) then Some(num.plus(x, num.one)) else Option.empty
    }

    trait SucceedingInfinite[E] extends Discrete.Succeeding.Infinite[E] {

      protected def num: Numeric[E]

      override def successor(x: E): E = num.plus(x, num.one)
    }

    trait Preceding[E] extends Discrete.Preceding[E] {

      protected def num: Numeric[E]

      override def predecessorOrNone(x: E): Discrete.Maybe[E] = 
        if hasPredecessor(x) then num.minus(x, num.one) else Discrete.None

      override def predecessorOpt(x: E): Option[E] =
        if hasPredecessor(x) then Some(num.minus(x, num.one)) else Option.empty
    }

    trait PrecedingInfinite[E] extends Discrete.Preceding.Infinite[E] {

      protected def num: Numeric[E]

      override def predecessor(x: E): E = num.minus(x, num.one)
    }

    trait Discrete[E] extends ordset.Discrete[E] with Succeeding[E] with Preceding[E]

    trait DiscreteInfinite[E] extends Discrete.Infinite[E] with SucceedingInfinite[E] with PrecedingInfinite[E]
  }

  object byte {

    def tryNaturalOrderWithBounds[L <: Byte, U <: Byte](
      lowerBound: L,
      upperBound: U
    ): Try[NaturalOrderWithBounds[L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, upperBound)))

    trait NaturalBounds extends Bounded.Including[Byte, Byte] {

      override val lowerBound: Byte = Byte.MinValue

      override val upperBound: Byte = Byte.MaxValue
    }

    class NaturalOrder()
      extends cats.kernel.instances.ByteOrder
      with DiscreteOrder.Finite[Byte, Byte, Byte]
      with numeric.Discrete[Byte]
      with NaturalBounds {

      override protected val num: Numeric[Byte] = implicitly[Numeric[Byte]]
    }

    class NaturalOrderWithBounds[+L <: Byte, +U <: Byte](
      override val lowerBound: L,
      override val upperBound: U
    ) extends cats.kernel.instances.ByteOrder
      with DiscreteOrder.Finite[Byte, L, U]
      with numeric.Discrete[Byte] {

      override protected val num: Numeric[Byte] = implicitly[Numeric[Byte]]
    }
  }

  object short {

    def tryNaturalOrderWithBounds[L <: Short, U <: Short](
      lowerBound: L,
      upperBound: U
    ): Try[NaturalOrderWithBounds[L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, upperBound)))


    trait NaturalBounds extends Bounded.Including[Short, Short] {

      override val lowerBound: Short = Short.MinValue

      override val upperBound: Short = Short.MaxValue
    }

    class NaturalOrder()
      extends cats.kernel.instances.ShortOrder
      with DiscreteOrder.Finite[Short, Short, Short]
      with numeric.Discrete[Short]
      with NaturalBounds {

      override protected val num: Numeric[Short] = implicitly[Numeric[Short]]
    }

    class NaturalOrderWithBounds[+L <: Short, +U <: Short](
      override val lowerBound: L,
      override val upperBound: U
    ) extends cats.kernel.instances.ShortOrder
      with DiscreteOrder.Finite[Short, L, U]
      with numeric.Discrete[Short] {

      override protected val num: Numeric[Short] = implicitly[Numeric[Short]]
    }
  }

  object int {

    def tryNaturalOrderWithBounds[L <: Int, U <: Int](
      lowerBound: L,
      upperBound: U
    ): Try[NaturalOrderWithBounds[L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, upperBound)))

    trait NaturalBounds extends Bounded.Including[Int, Int] {

      override val lowerBound: Int = Int.MinValue

      override val upperBound: Int = Int.MaxValue
    }

    class NaturalOrder()
      extends cats.kernel.instances.IntOrder
      with DiscreteOrder.Finite[Int, Int, Int]
      with numeric.Discrete[Int]
      with NaturalBounds {

      override protected val num: Numeric[Int] = implicitly[Numeric[Int]]
    }

    class NaturalOrderWithBounds[+L <: Int, +U <: Int](
      override val lowerBound: L,
      override val upperBound: U
    ) extends cats.kernel.instances.IntOrder
      with DiscreteOrder.Finite[Int, L, U]
      with numeric.Discrete[Int] {

      override protected val num: Numeric[Int] = implicitly[Numeric[Int]]
    }
  }

  object long {

    def tryNaturalOrderWithBounds[L <: Long, U <: Long](
      lowerBound: L,
      upperBound: U
    ): Try[NaturalOrderWithBounds[L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, upperBound)))

    trait NaturalBounds extends Bounded.Including[Long, Long] {

      override val lowerBound: Long = Long.MinValue

      override val upperBound: Long = Long.MaxValue
    }

    class NaturalOrder()
      extends cats.kernel.instances.LongOrder
      with DiscreteOrder.Finite[Long, Long, Long]
      with numeric.Discrete[Long]
      with NaturalBounds {

      override protected val num: Numeric[Long] = implicitly[Numeric[Long]]
    }

    class NaturalOrderWithBounds[+L <: Long, +U <: Long](
      override val lowerBound: L,
      override val upperBound: U
    ) extends cats.kernel.instances.LongOrder
      with DiscreteOrder.Finite[Long, L, U]
      with numeric.Discrete[Long] {

      override protected val num: Numeric[Long] = implicitly[Numeric[Long]]
    }
  }

  object bigInt {

    def tryNaturalOrderWithBounds[L <: BigInt, U <: BigInt](
      lowerBound: L,
      upperBound: U
    ): Try[NaturalOrderWithBounds[L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, upperBound)))

    class NaturalOrder()
      extends cats.kernel.instances.BigIntOrder
      with DiscreteOrder.InfiniteUnbounded[BigInt]
      with numeric.DiscreteInfinite[BigInt] {

      override protected val num: Numeric[BigInt] = implicitly[Numeric[BigInt]]
    }

    class NaturalOrderWithBounds[+L <: BigInt, +U <: BigInt](
      override val lowerBound: L,
      override val upperBound: U
    ) extends cats.kernel.instances.BigIntOrder
      with DiscreteOrder.Finite[BigInt, L, U]
      with numeric.Discrete[BigInt] {

      override protected val num: Numeric[BigInt] = implicitly[Numeric[BigInt]]
    }
  }

  object float {

    def tryNaturalOrderWithBounds[L <: Float, U <: Float](
      lowerBound: L,
      lowerBoundIncluded: Boolean,
      upperBound: U,
      upperBoundIncluded: Boolean
    ): Try[NaturalOrderWithBounds[L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, lowerBoundIncluded, upperBound, upperBoundIncluded)))

    trait NaturalBounds extends Bounded.Including[Float, Float] {

      override val lowerBound: Float = Float.MinValue

      override val upperBound: Float = Float.MaxValue
    }

    class NaturalOrder()
      extends cats.kernel.instances.FloatOrder
      with BoundedOrder.Including[Float, Float, Float]
      with NaturalBounds

    class NaturalOrderWithBounds[+L <: Float, +U <: Float](
      override val lowerBound: L,
      override val lowerBoundIncluded: Boolean,
      override val upperBound: U,
      override val upperBoundIncluded: Boolean
    ) extends cats.kernel.instances.FloatOrder
      with BoundedOrder[Float, L, U]
  }

  object double {

    def tryNaturalOrderWithBounds[L <: Double, U <: Double](
      lowerBound: L,
      lowerBoundIncluded: Boolean,
      upperBound: U,
      upperBoundIncluded: Boolean
    ): Try[NaturalOrderWithBounds[L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, lowerBoundIncluded, upperBound, upperBoundIncluded)))

    trait NaturalBounds extends Bounded.Including[Double, Double] {

      override val lowerBound: Double = Double.MinValue

      override val upperBound: Double = Double.MaxValue
    }

    class NaturalOrder()
      extends cats.kernel.instances.DoubleOrder
      with BoundedOrder.Including[Double, Double, Double]
      with NaturalBounds

    class NaturalOrderWithBounds[+L <: Double, +U <: Double](
      override val lowerBound: L,
      override val lowerBoundIncluded: Boolean,
      override val upperBound: U,
      override val upperBoundIncluded: Boolean
    ) extends cats.kernel.instances.DoubleOrder
      with BoundedOrder[Double, L, U]
  }

  object bigDecimal {

    def tryNaturalOrderWithBounds[L <: BigDecimal, U <: BigDecimal](
      lowerBound: L,
      lowerBoundIncluded: Boolean,
      upperBound: U,
      upperBoundIncluded: Boolean
    ): Try[NaturalOrderWithBounds[L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, lowerBoundIncluded, upperBound, upperBoundIncluded)))

    class NaturalOrder() extends cats.kernel.instances.BigDecimalOrder

    class NaturalOrderWithBounds[+L <: BigDecimal, +U <: BigDecimal](
      override val lowerBound: L,
      override val lowerBoundIncluded: Boolean,
      override val upperBound: U,
      override val upperBoundIncluded: Boolean
    ) extends cats.kernel.instances.BigDecimalOrder
      with BoundedOrder[BigDecimal, L, U]
  }

  object char {

    def tryNaturalOrderWithBounds[L <: Char, U <: Char](
      lowerBound: L,
      upperBound: U
    ): Try[NaturalOrderWithBounds[L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, upperBound)))

    trait NaturalBounds extends Bounded.Including[Char, Char] {

      override val lowerBound: Char = Char.MinValue

      override val upperBound: Char = Char.MaxValue
    }

    trait Succeeding extends Discrete.Succeeding[Char] {

      override def successorOrNone(x: Char): Discrete.Maybe[Char] = 
        if hasSuccessor(x) then (x + 1).toChar else Discrete.None

      override def successorOpt(x: Char): Option[Char] =
        if hasSuccessor(x) then Some((x + 1).toChar) else Option.empty
    }

    trait Preceding extends Discrete.Preceding[Char] {

      override def predecessorOrNone(x: Char): Discrete.Maybe[Char] = 
        if hasPredecessor(x) then (x - 1).toChar else Discrete.None

      override def predecessorOpt(x: Char): Option[Char] =
        if hasPredecessor(x) then Some((x - 1).toChar) else Option.empty
    }

    trait Discrete extends ordset.Discrete[Char] with Succeeding with Preceding

    class NaturalOrder()
      extends cats.kernel.instances.CharOrder
      with DiscreteOrder.Finite[Char, Char, Char]
      with Discrete
      with NaturalBounds

    class NaturalOrderWithBounds[+L <: Char, +U <: Char](
      override val lowerBound: L,
      override val upperBound: U
    ) extends cats.kernel.instances.CharOrder
      with DiscreteOrder.Finite[Char, L, U]
      with Discrete
  }

  object string {

    def tryNaturalOrderWithBounds[L <: String, U <: String](
      lowerBound: L,
      lowerBoundIncluded: Boolean,
      upperBound: U,
      upperBoundIncluded: Boolean
    ): Try[NaturalOrderWithBounds[L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, lowerBoundIncluded, upperBound, upperBoundIncluded)))

    trait NaturalBounds extends Bounded.Below.Including[String] {

      override val lowerBound: String = ""
    }

    class NaturalOrder()
      extends cats.kernel.instances.StringOrder
      with BoundedOrder.Below.Including[String, String]
      with NaturalBounds

    class NaturalOrderWithBounds[+L <: String, +U <: String](
      override val lowerBound: L,
      override val lowerBoundIncluded: Boolean,
      override val upperBound: U,
      override val upperBoundIncluded: Boolean
    ) extends cats.kernel.instances.StringOrder
      with BoundedOrder[String, L, U]
  }

  object iterable {

    def tryNaturalOrderWithBounds[T, L <: Iterable[T], U <: Iterable[T]](
      lowerBound: L,
      lowerBoundIncluded: Boolean,
      upperBound: U,
      upperBoundIncluded: Boolean
    )(
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ): Try[NaturalOrderWithBounds[T, L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, lowerBoundIncluded, upperBound, upperBoundIncluded)))

    trait NaturalBounds extends Bounded.Below.Including[Iterable[Nothing]] {

      override val lowerBound: Iterable[Nothing] = Nil
    }

    class NaturalOrder[T](
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ) extends IterableOrder[T](ord, hash)
      with BoundedOrder.Below.Including[Iterable[T], Iterable[Nothing]]
      with NaturalBounds

    class NaturalOrderWithBounds[T, +L <: Iterable[T], U <: Iterable[T]](
      override val lowerBound: L,
      override val lowerBoundIncluded: Boolean,
      override val upperBound: U,
      override val upperBoundIncluded: Boolean
    )(
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ) extends IterableOrder[T](ord, hash)
      with BoundedOrder[Iterable[T], L, U]

    class IterableEq[T](
      implicit protected val eq: Eq[T]
    ) extends Eq[Iterable[T]] {

      import ordset.util.IterableUtil

      override def eqv(x: Iterable[T], y: Iterable[T]): Boolean =
        IterableUtil.iterableEq(x, y)
    }

    class IterableHash[T](
      implicit override protected val eq: Hash[T]
    ) extends IterableEq[T] 
      with Hash[Iterable[T]] {
      
      import ordset.util.IterableUtil

      override def hash(x: Iterable[T]): Int =
        IterableUtil.iterableHash(x)
    }

    class IterableShow[T](
      implicit show: Show[T]
    ) extends Show[Iterable[T]] {

      override def show(x: Iterable[T]): String = 
        x.iterator.map(show.show(_)).mkString("Iterable(", ", ", ")")
    }

    // Protected section -------------------------------------------------------- //
    protected abstract class IterableOrder[T](
      ord: Order[T],
      hash: Hash[T]
    ) extends Order[Iterable[T]] 
      with Hash[Iterable[T]] {

      import ordset.util.IterableUtil

      override def compare(xs: Iterable[T], ys: Iterable[T]): Int =
        if (xs eq ys) 0
        else IterableUtil.iterableCompare(xs, ys)(ord)

      override def eqv(xs: Iterable[T], ys: Iterable[T]): Boolean =
        (xs eq ys) || IterableUtil.iterableEq(xs, ys)(hash)

      override def hash(x: Iterable[T]): Int = 
        IterableUtil.iterableHash(x)(hash)
    }
  }

  object list {

    def tryNaturalOrderWithBounds[T, L <: List[T], U <: List[T]](
      lowerBound: L,
      lowerBoundIncluded: Boolean,
      upperBound: U,
      upperBoundIncluded: Boolean
    )(
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ): Try[NaturalOrderWithBounds[T, L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, lowerBoundIncluded, upperBound, upperBoundIncluded)))

    trait NaturalBounds extends Bounded.Below.Including[Nil.type] {

      override val lowerBound: Nil.type = Nil
    }

    class NaturalOrder[T](
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ) extends cats.kernel.instances.ListOrder[T]
      with Hash[List[T]]
      with BoundedOrder.Below.Including[List[T], Nil.type]
      with NaturalBounds {

      override def hash(x: List[T]): Int = cats.kernel.instances.StaticMethods.listHash(x)
    }

    class NaturalOrderWithBounds[T, +L <: List[T], U <: List[T]](
      override val lowerBound: L,
      override val lowerBoundIncluded: Boolean,
      override val upperBound: U,
      override val upperBoundIncluded: Boolean
    )(
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ) extends cats.kernel.instances.ListOrder[T]
      with Hash[List[T]]
      with BoundedOrder[List[T], L, U] {

      override def hash(x: List[T]): Int = cats.kernel.instances.StaticMethods.listHash(x)
    }
  }

  object lazyList {

    def tryNaturalOrderWithBounds[T, L <: LazyList[T], U <: LazyList[T]](
      lowerBound: L,
      lowerBoundIncluded: Boolean,
      upperBound: U,
      upperBoundIncluded: Boolean
    )(
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ): Try[NaturalOrderWithBounds[T, L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, lowerBoundIncluded, upperBound, upperBoundIncluded)))

    trait NaturalBounds extends Bounded.Below.Including[LazyList[Nothing]] {

      override val lowerBound: LazyList[Nothing] = LazyList.empty
    }

    class NaturalOrder[T](
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ) extends cats.kernel.instances.LazyListOrder[T]
      with Hash[LazyList[T]]
      with BoundedOrder.Below.Including[LazyList[T], LazyList[Nothing]]
      with NaturalBounds {

      override def hash(x: LazyList[T]): Int = cats.kernel.instances.StaticMethods.orderedHash(x)
    }

    class NaturalOrderWithBounds[T, +L <: LazyList[T], U <: LazyList[T]](
      override val lowerBound: L,
      override val lowerBoundIncluded: Boolean,
      override val upperBound: U,
      override val upperBoundIncluded: Boolean
    )(
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ) extends cats.kernel.instances.LazyListOrder[T]
      with Hash[LazyList[T]]
      with BoundedOrder[LazyList[T], L, U] {

      override def hash(x: LazyList[T]): Int = cats.kernel.instances.StaticMethods.orderedHash(x)
    }
  }

  object queue {

    def tryNaturalOrderWithBounds[T, L <: Queue[T], U <: Queue[T]](
      lowerBound: L,
      lowerBoundIncluded: Boolean,
      upperBound: U,
      upperBoundIncluded: Boolean
    )(
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ): Try[NaturalOrderWithBounds[T, L, U]] =
      Try(validateBounds(new NaturalOrderWithBounds(lowerBound, lowerBoundIncluded, upperBound, upperBoundIncluded)))

    trait NaturalBounds extends Bounded.Below.Including[Queue[Nothing]] {

      override val lowerBound: Queue[Nothing] = Queue.empty
    }

    class NaturalOrder[T](
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ) extends cats.kernel.instances.QueueOrder[T]
      with Hash[Queue[T]]
      with BoundedOrder.Below.Including[Queue[T], Queue[Nothing]]
      with NaturalBounds {

      override def hash(x: Queue[T]): Int = cats.kernel.instances.StaticMethods.orderedHash(x)
    }

    class NaturalOrderWithBounds[T, +L <: Queue[T], U <: Queue[T]](
      override val lowerBound: L,
      override val lowerBoundIncluded: Boolean,
      override val upperBound: U,
      override val upperBoundIncluded: Boolean
    )(
      implicit 
      ord: Order[T],
      hash: Hash[T]
    ) extends cats.kernel.instances.QueueOrder[T]
      with Hash[Queue[T]]
      with BoundedOrder[Queue[T], L, U] {

      override def hash(x: Queue[T]): Int = cats.kernel.instances.StaticMethods.orderedHash(x)
    }
  }
}
