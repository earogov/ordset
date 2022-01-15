package ordset.core.domain

import ordset.{BoundedOrder, DiscreteOrder, Order, Hash}
import ordset.core.{Bound, ExtendedBound}
import ordset.core.range.{SimpleRange, SimpleRangeFactory, RangeAlgebra}

sealed trait Domain[E] extends DomainLike[E] {

  override implicit val rangeAlgebra: RangeAlgebra[ExtendedBound[E]] = RangeAlgebra.defaultAlgebra
}

object Domain {

  implicit def defaultHash[E, D <: Domain[E]]: Hash[D] = defaultHashInstance.asInstanceOf

  sealed trait Unbounded[E] extends Domain[E] {

    override implicit def boundOrd: Bound.DefaultOrder[E]

    override implicit def extendedOrd: ExtendedBound.UnboundedOrder[E]

    override def lowerBound: ExtendedBound.BelowAll.type = extendedOrd.lowerBound

    override def upperBound: ExtendedBound.AboveAll.type = extendedOrd.upperBound

    final override val isUnbounded: Boolean = true

    final override val isBoundedBelow: Boolean = false

    final override val isBoundedAbove: Boolean = false

    final override val isBounded: Boolean = false
  }

  sealed trait BoundedBelow[E] extends Domain[E] {

    override implicit def elementOrd: BoundedOrder.Below[E, E] with Hash[E]

    override implicit def boundOrd: Bound.BoundedBelowOrder[E]

    override implicit def extendedOrd: ExtendedBound.BoundedBelowOrder[E]

    override def lowerBound: Bound.Lower[E] = extendedOrd.lowerBound

    override def upperBound: ExtendedBound.AboveAll.type = extendedOrd.upperBound

    final override val isUnbounded: Boolean = false

    final override val isBoundedBelow: Boolean = true

    final override val isBoundedAbove: Boolean = false

    final override val isBounded: Boolean = true
  }

  sealed trait BoundedAbove[E] extends Domain[E] {

    override implicit def elementOrd: BoundedOrder.Above[E, E] with Hash[E]

    override implicit def boundOrd: Bound.BoundedAboveOrder[E]

    override implicit def extendedOrd: ExtendedBound.BoundedAboveOrder[E]

    override def lowerBound: ExtendedBound.BelowAll.type = extendedOrd.lowerBound

    override def upperBound: Bound.Upper[E] = extendedOrd.upperBound

    final override val isUnbounded: Boolean = false

    final override val isBoundedBelow: Boolean = false

    final override val isBoundedAbove: Boolean = true

    final override val isBounded: Boolean = true
  }

  sealed trait Bounded[E] extends Domain[E] {

    override implicit def elementOrd: BoundedOrder[E, E, E] with Hash[E]

    override implicit def boundOrd: Bound.BoundedOrder[E]

    override implicit def extendedOrd: ExtendedBound.BoundedOrder[E]

    override def lowerBound: Bound.Lower[E] = extendedOrd.lowerBound

    override def upperBound: Bound.Upper[E] = extendedOrd.upperBound

    final override val isUnbounded: Boolean = false

    final override val isBoundedBelow: Boolean = true

    final override val isBoundedAbove: Boolean = true
    
    final override val isBounded: Boolean = true
  }

  sealed trait Continuous[E] extends Domain[E] {

    override implicit def boundOrd: Bound.ContinuousOrder[E]

    final override val isContinuous: Boolean = true

    final override val isDiscrete: Boolean = false
  }

  sealed trait Discrete[E] extends Domain[E] {

    override implicit def elementOrd: DiscreteOrder[E] with Hash[E]

    override implicit def boundOrd: Bound.DiscreteOrder[E]

    final override val isContinuous: Boolean = false

    final override val isDiscrete: Boolean = true
  }

  trait ContinuousUnbounded[E] extends Unbounded[E] with Continuous[E] {

    override implicit def boundOrd: Bound.ContinuousUnboundedOrder[E]
  }

  object ContinuousUnbounded {

    implicit def default[E](implicit elementOrd: Order[E] with Hash[E]): ContinuousUnbounded[E] = 
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](ord: Order[E] with Hash[E]) extends ContinuousUnbounded[E] {

      override implicit val elementOrd: Order[E] with Hash[E] = ord

      override implicit val boundOrd: Bound.ContinuousUnboundedOrder[E] = 
        Bound.continuousUnboundedOrder(elementOrd)
      
      override implicit val extendedOrd: ExtendedBound.UnboundedOrder[E] =
        ExtendedBound.unboundedOrder(boundOrd)

      override implicit val rangeFactory: SimpleRangeFactory[ExtendedBound[E]] = 
        SimpleRangeFactory.unbounded(extendedOrd)

      // override val boundsRange: SimpleRange[ExtendedBound[E]] = 
      //   rangeFactory.between(lowerBound, upperBound)
    }
  }

  trait ContinuousBoundedBelow[E] extends BoundedBelow[E] with Continuous[E] {

    override implicit def boundOrd: Bound.ContinuousBoundedBelowOrder[E]
  }

  object ContinuousBoundedBelow {

    implicit def default[E](implicit elementOrd: BoundedOrder.Below[E, E] with Hash[E]): ContinuousBoundedBelow[E] =
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](ord: BoundedOrder.Below[E, E] with Hash[E]) extends ContinuousBoundedBelow[E] {

      override implicit val elementOrd: BoundedOrder.Below[E, E] with Hash[E] = ord

      override implicit val boundOrd: Bound.ContinuousBoundedBelowOrder[E] =
        Bound.continuousBoundedBelowOrder(elementOrd)

      override implicit val extendedOrd: ExtendedBound.BoundedBelowOrder[E] =
        ExtendedBound.boundedBelowOrder(boundOrd)

      override implicit val rangeFactory: SimpleRangeFactory[ExtendedBound[E]] = 
        SimpleRangeFactory.boundedBelow(extendedOrd)

      // override val boundsRange: SimpleRange[ExtendedBound[E]] = 
      //   rangeFactory.between(lowerBound, upperBound)
    }
  }

  trait ContinuousBoundedAbove[E] extends BoundedAbove[E] with Continuous[E] {

    override implicit def boundOrd: Bound.ContinuousBoundedAboveOrder[E]
  }

  object ContinuousBoundedAbove {

    implicit def default[E](implicit elementOrd: BoundedOrder.Above[E, E] with Hash[E]): ContinuousBoundedAbove[E] =
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](ord: BoundedOrder.Above[E, E] with Hash[E]) extends ContinuousBoundedAbove[E] {

      override implicit val elementOrd: BoundedOrder.Above[E, E] with Hash[E] = ord

      override implicit val boundOrd: Bound.ContinuousBoundedAboveOrder[E] =
        Bound.continuousBoundedAboveOrder(elementOrd)

      override implicit val extendedOrd: ExtendedBound.BoundedAboveOrder[E] =
        ExtendedBound.boundedAboveOrder(boundOrd)

      override implicit val rangeFactory: SimpleRangeFactory[ExtendedBound[E]] = 
        SimpleRangeFactory.boundedAbove(extendedOrd)

      // override val boundsRange: SimpleRange[ExtendedBound[E]] = 
      //   rangeFactory.between(lowerBound, upperBound)
    }
  }

  trait ContinuousBounded[E] extends Bounded[E] with Continuous[E] {

    override implicit def boundOrd: Bound.ContinuousBoundedOrder[E]
  }

  object ContinuousBounded {

    implicit def default[E](implicit elementOrd: BoundedOrder[E, E, E] with Hash[E]): ContinuousBounded[E] =
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](ord: BoundedOrder[E, E, E] with Hash[E]) extends ContinuousBounded[E] {

      override implicit val elementOrd: BoundedOrder[E, E, E] with Hash[E] = ord

      override implicit val boundOrd: Bound.ContinuousBoundedOrder[E] =
        Bound.continuousBoundedOrder(elementOrd)

      override implicit val extendedOrd: ExtendedBound.BoundedOrder[E] =
        ExtendedBound.boundedOrder(boundOrd)

      override implicit val rangeFactory: SimpleRangeFactory[ExtendedBound[E]] = 
        SimpleRangeFactory.bounded(extendedOrd)

      // override val boundsRange: SimpleRange[ExtendedBound[E]] = 
      //   rangeFactory.between(lowerBound, upperBound)
    }
  }

  trait DiscreteUnbounded[E] extends Unbounded[E] with Discrete[E] {

    override implicit def elementOrd: DiscreteOrder[E] with ordset.Discrete.Infinite[E] with Hash[E]

    override implicit def boundOrd: Bound.DiscreteUnboundedOrder[E]
  }

  object DiscreteUnbounded {

    implicit def default[E](
      implicit elementOrd: DiscreteOrder[E] with ordset.Discrete.Infinite[E] with Hash[E]
    ): DiscreteUnbounded[E] =
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](
      ord: DiscreteOrder[E] with ordset.Discrete.Infinite[E] with Hash[E]
    ) extends DiscreteUnbounded[E] {

      override implicit val elementOrd: DiscreteOrder[E] with ordset.Discrete.Infinite[E] with Hash[E] = ord

      override implicit val boundOrd: Bound.DiscreteUnboundedOrder[E] = 
        Bound.discreteUnboundedOrder(elementOrd)

      override implicit val extendedOrd: ExtendedBound.UnboundedOrder[E] =
        ExtendedBound.unboundedOrder(boundOrd)

      override implicit val rangeFactory: SimpleRangeFactory[ExtendedBound[E]] = 
        SimpleRangeFactory.unbounded(extendedOrd)
    }
  }

  trait DiscreteBoundedBelow[E] extends BoundedBelow[E] with Discrete[E] {

    override implicit def elementOrd: BoundedOrder.Below[E, E] with DiscreteOrder[E] with Hash[E]

    override implicit def boundOrd: Bound.DiscreteBoundedBelowOrder[E]
  }

  object DiscreteBoundedBelow {

    implicit def default[E](
      implicit elementOrd: BoundedOrder.Below[E, E] with DiscreteOrder[E] with Hash[E]
    ): DiscreteBoundedBelow[E] =
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](
      ord: BoundedOrder.Below[E, E] with DiscreteOrder[E] with Hash[E]
    ) extends DiscreteBoundedBelow[E] {

      override implicit val elementOrd: BoundedOrder.Below[E, E] with DiscreteOrder[E] with Hash[E] = ord

      override implicit val boundOrd: Bound.DiscreteBoundedBelowOrder[E] =
        Bound.discreteBoundedBelowOrder(elementOrd)

      override implicit val extendedOrd: ExtendedBound.BoundedBelowOrder[E] =
        ExtendedBound.boundedBelowOrder(boundOrd)

      override implicit val rangeFactory: SimpleRangeFactory[ExtendedBound[E]] = 
        SimpleRangeFactory.boundedBelow(extendedOrd)
    }
  }

  trait DiscreteBoundedAbove[E] extends BoundedAbove[E] with Discrete[E] {

    override implicit def elementOrd: BoundedOrder.Above[E, E] with DiscreteOrder[E] with Hash[E]

    override implicit def boundOrd: Bound.DiscreteBoundedAboveOrder[E]
  }

  object DiscreteBoundedAbove {

    implicit def default[E](
      implicit elementOrd: BoundedOrder.Above[E, E] with DiscreteOrder[E] with Hash[E]
    ): DiscreteBoundedAbove[E] =
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](
      ord: BoundedOrder.Above[E, E] with DiscreteOrder[E] with Hash[E]
    ) extends DiscreteBoundedAbove[E] {

      override implicit val elementOrd: BoundedOrder.Above[E, E] with DiscreteOrder[E] with Hash[E] = ord

      override implicit val boundOrd: Bound.DiscreteBoundedAboveOrder[E] =
        Bound.discreteBoundedAboveOrder(elementOrd)

      override implicit val extendedOrd: ExtendedBound.BoundedAboveOrder[E] =
        ExtendedBound.boundedAboveOrder(boundOrd)

      override implicit val rangeFactory: SimpleRangeFactory[ExtendedBound[E]] = 
        SimpleRangeFactory.boundedAbove(extendedOrd)
    }
  }

  trait DiscreteBounded[E] extends Bounded[E] with Discrete[E] {

    override implicit def elementOrd: BoundedOrder[E, E, E] with DiscreteOrder[E] with Hash[E]

    override implicit def boundOrd: Bound.DiscreteBoundedOrder[E]
  }

  object DiscreteBounded {

    implicit def default[E](
      implicit elementOrd: BoundedOrder[E, E, E] with DiscreteOrder[E] with Hash[E]
    ): DiscreteBounded[E] =
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](
      ord: BoundedOrder[E, E, E] with DiscreteOrder[E] with Hash[E]
    ) extends DiscreteBounded[E] {

      override implicit val elementOrd: BoundedOrder[E, E, E] with DiscreteOrder[E] with Hash[E] = ord

      override implicit val boundOrd: Bound.DiscreteBoundedOrder[E] =
        Bound.discreteBoundedOrder(elementOrd)

      override implicit val extendedOrd: ExtendedBound.BoundedOrder[E] =
        ExtendedBound.boundedOrder(boundOrd)

      override implicit val rangeFactory: SimpleRangeFactory[ExtendedBound[E]] = 
        SimpleRangeFactory.bounded(extendedOrd)
    }
  }

  class DefaultHash[E, D <: Domain[E]](
    val orderHash: Hash[Order[E]]
  ) extends Hash[D] {

    import ordset.util.HashUtil._

    private val hashConst: Array[Int] = Array(0xC38B071A, 0x084E22F0, 0x4FE710A5, 0xAA63B51C, 0x84CE051B, 0x6E78FA31)

    override def eqv(x: D, y: D): Boolean =
      (x eq y) || 
      (
        x.isBoundedBelow == y.isBoundedBelow && 
        x.isBoundedAbove == y.isBoundedAbove && 
        x.isContinuous == y.isContinuous && 
        orderHash.eqv(x.elementOrd, y.elementOrd)
      )

    override def hash(x: D): Int = {
      val h1 = if (x.isBoundedBelow) hashConst(0) else hashConst(1)
      val h2 = if (x.isBoundedAbove) hashConst(2) else hashConst(3)
      val h3 = if (x.isContinuous) hashConst(4) else hashConst(5)
      product4Hash(orderHash.hash(x.elementOrd), h1, h2, h3)
    }
  }

  // Private section ---------------------------------------------------------- //
  private val defaultHashInstance: DefaultHash[Any, Domain[Any]] = 
    new DefaultHash(ordset.util.HashUtil.classBasedHash)
}