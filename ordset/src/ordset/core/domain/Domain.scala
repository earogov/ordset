package ordset.core.domain

import ordset.{BoundedOrder, DiscreteOrder, DiscreteFiniteOrder, Order, Hash}
import ordset.core.{Bound, ExtendedBound}

sealed trait Domain[E] extends DomainLike[E]

object Domain {

  implicit def defaultHash[E, D <: Domain[E]]: Hash[D] = defaultHashInstance.asInstanceOf

  sealed trait Unbounded[E] extends Domain[E] {

    override implicit def boundOrd: Bound.DefaultUnboundedOrder[E]

    override implicit def extendedOrd: ExtendedBound.DefaultUnboundedOrder[E]

    override def lowerExtendedBound: ExtendedBound.BelowAll.type = extendedOrd.lowerBound

    override def upperExtendedBound: ExtendedBound.AboveAll.type = extendedOrd.upperBound

    final override val isUnbounded: Boolean = false

    final override val isBounded: Boolean = true
  }

  sealed trait Bounded[E] extends Domain[E] {

    override implicit def elementOrd: BoundedOrder[E, E, E] with Hash[E]

    override implicit def boundOrd: Bound.DefaultBoundedOrder[E]

    override implicit def extendedOrd: ExtendedBound.DefaultBoundedOrder[E]

    override def lowerExtendedBound: Bound.Lower[E] = extendedOrd.lowerBound

    override def upperExtendedBound: Bound.Upper[E] = extendedOrd.upperBound

    final override val isUnbounded: Boolean = true

    final override val isBounded: Boolean = false
  }

  sealed trait Continuous[E] extends Domain[E] {

    final override val isContinuos: Boolean = true

    final override val isDiscrete: Boolean = false
  }

  sealed trait Discrete[E] extends Domain[E] {

    override implicit def elementOrd: DiscreteOrder[E] with Hash[E]

    final override val isContinuos: Boolean = false

    final override val isDiscrete: Boolean = true
  }

  trait UnboundedContinuous[E] extends Unbounded[E] with Continuous[E]

  object UnboundedContinuous {

    implicit def default[E](implicit elementOrd: Order[E] with Hash[E]): UnboundedContinuous[E] = 
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](ord: Order[E] with Hash[E]) extends UnboundedContinuous[E] {

      override implicit val elementOrd: Order[E] with Hash[E] = ord

      override implicit val boundOrd: Bound.DefaultUnboundedOrder[E] = 
        Bound.defaultUnboundedOrder(elementOrd)
      
      override implicit val extendedOrd: ExtendedBound.DefaultUnboundedOrder[E] =
        ExtendedBound.defaultUnboundedOrder(boundOrd)
    }
  }

  trait UnboundedDiscrete[E] extends Unbounded[E] with Discrete[E] {

    override implicit def elementOrd: DiscreteOrder[E] with Hash[E]
  }

  object UnboundedDiscrete {

    implicit def default[E](implicit elementOrd: DiscreteOrder[E] with Hash[E]): UnboundedDiscrete[E] =
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](ord: DiscreteOrder[E] with Hash[E]) extends UnboundedDiscrete[E] {

      override implicit val elementOrd: DiscreteOrder[E] with Hash[E] = ord

      override implicit val boundOrd: Bound.DefaultUnboundedOrder[E] = 
        Bound.defaultUnboundedOrder(elementOrd)

      override implicit val extendedOrd: ExtendedBound.DefaultUnboundedOrder[E] =
        ExtendedBound.defaultUnboundedOrder(boundOrd)
    }
  }

  trait BoundedContinuous[E] extends Bounded[E] with Continuous[E]

  object BoundedContinuous {

    implicit def default[E](implicit elementOrd: BoundedOrder[E, E, E] with Hash[E]): BoundedContinuous[E] =
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](ord: BoundedOrder[E, E, E] with Hash[E]) extends BoundedContinuous[E] {

      override implicit val elementOrd: BoundedOrder[E, E, E] with Hash[E] = ord

      override implicit val boundOrd: Bound.DefaultBoundedOrder[E] =
        Bound.defaultBoundedOrder(elementOrd)

      override implicit val extendedOrd: ExtendedBound.DefaultBoundedOrder[E] =
        ExtendedBound.defaultBoundedOrder(boundOrd)
    }
  }

  trait BoundedDiscrete[E] extends Bounded[E] with Discrete[E] {

    override implicit def elementOrd: DiscreteFiniteOrder[E, E, E] with Hash[E]
  }

  object BoundedDiscrete {

    implicit def default[E](implicit elementOrd: DiscreteFiniteOrder[E, E, E] with Hash[E]): BoundedDiscrete[E] =
      new DefaultImpl(elementOrd)

    class DefaultImpl[E](ord: DiscreteFiniteOrder[E, E, E] with Hash[E]) extends BoundedDiscrete[E] {

      override implicit val elementOrd: DiscreteFiniteOrder[E, E, E] with Hash[E] = ord

      override implicit val boundOrd: Bound.DefaultBoundedOrder[E] =
        Bound.defaultBoundedOrder(elementOrd)

      override implicit val extendedOrd: ExtendedBound.DefaultBoundedOrder[E] =
        ExtendedBound.defaultBoundedOrder(boundOrd)
    }
  }

  class DefaultHash[E, D <: Domain[E]](
    val orderHash: Hash[Order[E]]
  ) extends Hash[D] {

    import ordset.util.HashUtil._

    override def eqv(x: D, y: D): Boolean =
      (x eq y) || 
      (
        x.isUnbounded == y.isUnbounded && 
        x.isContinuos == y.isContinuos && 
        orderHash.eqv(x.elementOrd, y.elementOrd)
      )

    override def hash(x: D): Int = {
      val h1 = if (x.isUnbounded) 0xC3BB071A else 0x084E22F0
      val h2 = if (x.isContinuos) 0x84CE051B else 0x6E78FA31
      val h3 = product2Hash(orderHash.hash(x.elementOrd), h1)
      product2Hash(h3, h2)
    }
  }

  // Private section ---------------------------------------------------------- //
  private val defaultHashInstance: DefaultHash[Any, Domain[Any]] = 
    new DefaultHash(ordset.util.HashUtil.classBasedHash)
}