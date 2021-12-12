package ordset

import scala.collection.immutable.Queue

object BoundedComponents {

  trait BoundedBelowProxy[+L] extends Bounded.Below[L] {

    protected val original: Bounded.Below[L]

    override def lowerBound: L = original.lowerBound

    override def lowerBoundIncluded: Boolean = original.lowerBoundIncluded
  }

  trait BoundedAboveProxy[+U] extends Bounded.Above[U] {

    protected val original: Bounded.Above[U]

    override def upperBound: U = original.upperBound

    override def upperBoundIncluded: Boolean = original.upperBoundIncluded
  }

  trait BoundedProxy[+L, +U] 
    extends BoundedBelowProxy[L] 
    with BoundedAboveProxy[U] {

    override protected val original: Bounded[L, U]
  }

  object Reversed {

    trait BoundedBelowProxy[+L] extends Bounded.Below[L] {

      protected val original: Bounded.Above[L]

      override def lowerBound: L = original.upperBound

      override def lowerBoundIncluded: Boolean = original.upperBoundIncluded
    }

    trait BoundedAboveProxy[+U] extends Bounded.Above[U] {

      protected val original: Bounded.Below[U]

      override def upperBound: U = original.lowerBound

      override def upperBoundIncluded: Boolean = original.lowerBoundIncluded
    }

    trait BoundedProxy[+L, +U] 
      extends BoundedBelowProxy[L] 
      with BoundedAboveProxy[U] {

      override protected val original: Bounded[U, L]
    }
  }

  trait UnitBounded extends Bounded.Including[Unit, Unit] {

    override val lowerBound: Unit = {}

    override val upperBound: Unit = {}
  }

  trait BooleanBounded extends Bounded.Including[Boolean, Boolean] {

    override val lowerBound: Boolean = false

    override val upperBound: Boolean = true
  }

  trait ByteBounded extends Bounded.Including[Byte, Byte] {

    override val lowerBound: Byte = Byte.MinValue

    override val upperBound: Byte = Byte.MaxValue
  }

  trait ShortBounded extends Bounded.Including[Short, Short] {

    override val lowerBound: Short = Short.MinValue

    override val upperBound: Short = Short.MaxValue
  }

  trait IntBounded extends Bounded.Including[Int, Int] {

    override val lowerBound: Int = Int.MinValue

    override val upperBound: Int = Int.MaxValue
  }

  trait LongBounded extends Bounded.Including[Long, Long] {

    override val lowerBound: Long = Long.MinValue

    override val upperBound: Long = Long.MaxValue
  }

  trait FloatBounded extends Bounded.Including[Float, Float] {

    override val lowerBound: Float = Float.MinValue

    override val upperBound: Float = Float.MaxValue
  }

  trait DoubleBounded extends Bounded.Including[Double, Double] {

    override val lowerBound: Double = Double.MinValue

    override val upperBound: Double = Double.MaxValue
  }

  trait CharBounded extends Bounded.Including[Char, Char] {

    override val lowerBound: Char = Char.MinValue

    override val upperBound: Char = Char.MaxValue
  }

  trait StringBoundedBelow extends Bounded.Below.Including[String] {

    override val lowerBound: String = ""
  }

  trait ListBoundedBelow extends Bounded.Below.Including[Nil.type] {

    override val lowerBound: Nil.type = Nil
  }

  trait QueueBoundedBelow extends Bounded.Below.Including[Queue[Nothing]] {

    override val lowerBound: Queue[Nothing] = Queue.empty
  }

  trait LazyListBoundedBelow extends Bounded.Below.Including[LazyList[Nothing]] {

    override val lowerBound: LazyList[Nothing] = LazyList.empty
  }
}