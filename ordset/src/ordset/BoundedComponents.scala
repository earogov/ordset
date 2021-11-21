package ordset

object BoundedComponents {

  trait BoundedBelowProxy[+E] extends Bounded.Below[E] {

    protected val original: Bounded.Below[E]

    override def lowerBound: E = original.lowerBound

    override def lowerBoundIncluded: Boolean = original.lowerBoundIncluded
  }

  trait BoundedAboveProxy[+E] extends Bounded.Above[E] {

    protected val original: Bounded.Above[E]

    override def upperBound: E = original.upperBound

    override def upperBoundIncluded: Boolean = original.upperBoundIncluded
  }

  trait BoundedProxy[+E] 
    extends BoundedBelowProxy[E] 
    with BoundedAboveProxy[E] {

    override protected val original: Bounded[E]
  }

  object Reversed {

    trait BoundedBelowProxy[+E] extends Bounded.Below[E] {

      protected val original: Bounded.Above[E]

      override def lowerBound: E = original.upperBound

      override def lowerBoundIncluded: Boolean = original.upperBoundIncluded
    }

    trait BoundedAboveProxy[+E] extends Bounded.Above[E] {

      protected val original: Bounded.Below[E]

      override def upperBound: E = original.lowerBound

      override def upperBoundIncluded: Boolean = original.lowerBoundIncluded
    }

    trait BoundedProxy[+E] 
      extends BoundedBelowProxy[E] 
      with BoundedAboveProxy[E] {

      override protected val original: Bounded[E]
    }
  }

  trait UnitBounded extends Bounded.Including[Unit] {

    override val lowerBound: Unit = {}

    override val upperBound: Unit = {}
  }

  trait BooleanBounded extends Bounded.Including[Boolean] {

    override val lowerBound: Boolean = false

    override val upperBound: Boolean = true
  }

  trait ByteBounded extends Bounded.Including[Byte] {

    override val lowerBound: Byte = Byte.MinValue

    override val upperBound: Byte = Byte.MaxValue
  }

  trait ShortBounded extends Bounded.Including[Short] {

    override val lowerBound: Short = Short.MinValue

    override val upperBound: Short = Short.MaxValue
  }

  trait IntBounded extends Bounded.Including[Int] {

    override val lowerBound: Int = Int.MinValue

    override val upperBound: Int = Int.MaxValue
  }

  trait LongBounded extends Bounded.Including[Long] {

    override val lowerBound: Long = Long.MinValue

    override val upperBound: Long = Long.MaxValue
  }

  trait FloatBounded extends Bounded.Including[Float] {

    override val lowerBound: Float = Float.MinValue

    override val upperBound: Float = Float.MaxValue
  }

  trait DoubleBounded extends Bounded.Including[Double] {

    override val lowerBound: Double = Double.MinValue

    override val upperBound: Double = Double.MaxValue
  }
}