package ordset

import scala.annotation.unchecked.uncheckedVariance

/**
 * Contravariant version of [[BoundedOrder]] typeclass (see [[ContravariantOrder]] for details on contravariance and 
 * [[scala.annotation.unchecked.uncheckedVariance]] annotation).
 */ 
trait ContravariantBoundedOrder[-E, +L <: E, +U <: E] 
  extends BoundedOrder[E @uncheckedVariance, L, U]
  with ContravariantBoundedOrder.Below[E, L] 
  with ContravariantBoundedOrder.Above[E, U]
  with Reversible[ContravariantBoundedOrder[E, L, U], ContravariantBoundedOrder[E, U, L]] {

  override def reversed: ContravariantBoundedOrder[E, U, L] = new ContravariantBoundedOrder.ReversedImpl(this)
}

object ContravariantBoundedOrder {

  /**
   * Returns contravariant version of given [[BoundedOrder]] instance.
   */
  def fromOrder[E, L <: E, U <: E](ord: BoundedOrder[E, L, U]): ContravariantBoundedOrder[E, L, U] = 
    ord match {
      case ord: ContravariantBoundedOrder[E, L, U] => ord
      case _ => new ProxyImpl(ord)
    }

  /**
   *  Contravariant version of [[BoundedOrder.Below]].
   */ 
  trait Below[-E, +L <: E] 
    extends BoundedOrder.Below[E @uncheckedVariance, L]
    with ContravariantOrder[E] 
    with Reversible[Below[E, L], Above[E, L]] {

    override def reversed: Above[E, L] = new Above.ReversedImpl(this)
  }

  object Below {

    /**
     * Returns contravariant version of given [[BoundedOrder.Below]] instance.
     */
    def fromOrder[E, L <: E](ord: BoundedOrder.Below[E, L]): Below[E, L] = 
      ord match {
        case ord: ContravariantBoundedOrder.Below[E, L] => ord
        case _ => new ProxyImpl(ord)
      }

    /**
     *  Contravariant version of [[BoundedOrder.Below.Including]].
     */ 
    trait Including[-E, +L <: E] 
      extends BoundedOrder.Below.Including[E @uncheckedVariance, L]
      with Below[E, L] 
      with Reversible[Below.Including[E, L], Above.Including[E, L]] {

      override def reversed: Above.Including[E, L] = new Above.Including.ReversedImpl(this)
    }

    object Including {

      /**
       * Returns contravariant version of given [[BoundedOrder.Below.Including]] instance.
       */
      def fromOrder[E, L <: E](ord: BoundedOrder.Below.Including[E, L]): Below.Including[E, L] = 
        ord match {
          case ord: ContravariantBoundedOrder.Below.Including[E, L] => ord
          case _ => new ProxyImpl(ord)
        }

      /**
       * [[ContravariantBoundedOrder.Below.Including]] typeclass received by reverting 
       * [[ContravariantBoundedOrder.Above.Including]] instance.
       */
      trait Reversed[-E, +L <: E] 
        extends Below.Reversed[E, L] 
        with Bounded.Below.Including.Reversed[L] 
        with Below.Including[E, L]

      class ReversedImpl[E, +L <: E](original: Above.Including[E, L]) extends Reversed[E, L] {

        override val reversed: Above.Including[E, L] = original
      }

      /**
       * [[ContravariantBoundedOrder.Below.Including]] implementation delegating to another 
       * [[BoundedOrder.Below.Including]] instance.
       */
      trait Proxy[-E, +L <: E, EE >: E] 
        extends ContravariantOrder.Proxy[E, EE] 
        with Bounded.Below.Including.Proxy[L] 
        with Below.Including[E, L] {

        override protected def original: BoundedOrder.Below.Including[EE, L]
      }

      class ProxyImpl[-E, +L <: E, EE >: E](
        override val original: BoundedOrder.Below.Including[EE, L]
      ) extends Proxy[E, L, EE]
    }

    /**
     * [[ContravariantBoundedOrder.Below]] typeclass received by reverting 
     * [[ContravariantBoundedOrder.Above]] instance.
     */
    trait Reversed[-E, +L <: E] 
      extends ContravariantOrder.Reversed[E] 
      with Bounded.Below.Reversed[L] 
      with Below[E, L]

    class ReversedImpl[-E, +L <: E](original: Above[E, L]) extends Reversed[E, L] {

      override val reversed: Above[E, L] = original
    }

    /**
     * [[ContravariantBoundedOrder.Below]] implementation delegating to another 
     * [[BoundedOrder.Below]] instance.
     */
    trait Proxy[-E, +L <: E, EE >: E] 
      extends ContravariantOrder.Proxy[E, EE] 
      with Bounded.Below.Proxy[L] 
      with Below[E, L] {

      override protected def original: BoundedOrder.Below[EE, L]
    }

    class ProxyImpl[-E, +L <: E, EE >: E](override val original: BoundedOrder.Below[EE, L]) extends Proxy[E, L, EE]
  }

  /**
   *  Contravariant version of [[BoundedOrder.Above]].
   */ 
  trait Above[-E, +U <: E] 
    extends BoundedOrder.Above[E @uncheckedVariance, U] 
    with ContravariantOrder[E] 
    with Reversible[Above[E, U], Below[E, U]] {

    override def reversed: Below[E, U] = new Below.ReversedImpl(this)
  }

  object Above {

    /**
     * Returns contravariant version of given [[BoundedOrder.Above]] instance.
     */
    def fromOrder[E, U <: E](ord: BoundedOrder.Above[E, U]): Above[E, U] = 
      ord match {
        case ord: ContravariantBoundedOrder.Above[E, U] => ord
        case _ => new ProxyImpl(ord)
      }

    /**
     *  Contravariant version of [[BoundedOrder.Above.Including]].
     */ 
    trait Including[-E, +U <: E] 
      extends BoundedOrder.Above.Including[E @uncheckedVariance, U]
      with Above[E, U] 
      with Reversible[Above.Including[E, U], Below.Including[E, U]] {

      override def reversed: Below.Including[E, U] = new Below.Including.ReversedImpl(this)
    }

    object Including {

      /**
       * Returns contravariant version of given [[BoundedOrder.Above.Including]] instance.
       */
      def fromOrder[E, U <: E](ord: BoundedOrder.Above.Including[E, U]): Above.Including[E, U] = 
        ord match {
          case ord: ContravariantBoundedOrder.Above.Including[E, U] => ord
          case _ => new ProxyImpl(ord)
        }

      /**
       * [[ContravariantBoundedOrder.Above.Including]] typeclass received by reverting 
       * [[ContravariantBoundedOrder.Below.Including]] instance.
       */
      trait Reversed[-E, +U <: E] 
        extends Above.Reversed[E, U] 
        with Bounded.Above.Including.Reversed[U]
        with Above.Including[E, U]

      class ReversedImpl[-E, +U <: E](original: Below.Including[E, U]) extends Reversed[E, U] {

        override val reversed: Below.Including[E, U] = original
      }

      /**
       * [[ContravariantBoundedOrder.Above.Including]] implementation delegating to another 
       * [[BoundedOrder.Above.Including]] instance.
       */
      trait Proxy[-E, +U <: E, EE >: E] 
        extends ContravariantOrder.Proxy[E, EE] 
        with Bounded.Above.Including.Proxy[U] 
        with Above.Including[E, U] {

        override protected def original: BoundedOrder.Above.Including[EE, U]
      }

      class ProxyImpl[-E, +U <: E, EE >: E](
        override val original: BoundedOrder.Above.Including[EE, U]
      ) extends Proxy[E, U, EE]
    }

    /**
     * [[ContravariantBoundedOrder.Above]] typeclass received by reverting 
     * [[ContravariantBoundedOrder.Below]] instance.
     */
    trait Reversed[-E, +U <: E] 
      extends ContravariantOrder.Reversed[E] 
      with Bounded.Above.Reversed[U] with Above[E, U]

    class ReversedImpl[-E, +U <: E](original: Below[E, U]) extends Reversed[E, U] {

      override val reversed: Below[E, U] = original
    }

    /**
     * [[ContravariantBoundedOrder.Above]] implementation delegating to another 
     * [[BoundedOrder.Above]] instance.
     */
    trait Proxy[-E, +U <: E, EE >: E] 
      extends ContravariantOrder.Proxy[E, EE] 
      with Bounded.Above.Proxy[U] 
      with Above[E, U] {

      override protected def original: BoundedOrder.Above[EE, U]
    }

    class ProxyImpl[-E, +U <: E, EE >: E](override val original: BoundedOrder.Above[EE, U]) extends Proxy[E, U, EE]
  }

  /**
   *  Contravariant version of [[BoundedOrder.Including]].
   */ 
  trait Including[-E, +L <: E, +U <: E] 
    extends BoundedOrder.Including[E @uncheckedVariance, L, U]
    with ContravariantBoundedOrder[E, L, U]
    with Below.Including[E, L] 
    with Above.Including[E, U] 
    with Reversible[Including[E, L, U], Including[E, U, L]] {

    override def reversed: Including[E, U, L] = new Including.ReversedImpl(this)
  }

  object Including {

    /**
     * Returns contravariant version of given [[BoundedOrder.Including]] instance.
     */
    def fromOrder[E, L <: E, U <: E](ord: BoundedOrder.Including[E, L, U]): Including[E, L, U] = 
      ord match {
        case ord: ContravariantBoundedOrder.Including[E, L, U] => ord
        case _ => new ProxyImpl(ord)
      }

    /**
     * [[ContravariantBoundedOrder.Including]] typeclass received by reverting another 
     * [[ContravariantBoundedOrder.Including]] instance.
     */
    trait Reversed[-E, +L <: E, +U <: E]
      extends ContravariantBoundedOrder.Reversed[E, L, U]
      with Below.Including.Reversed[E, L]
      with Above.Including.Reversed[E, U]
      with Bounded.Including.Reversed[L, U]
      with Including[E, L, U]

    class ReversedImpl[-E, +L <: E, +U <: E](original: Including[E, U, L]) extends Reversed[E, L, U] {

      override val reversed: Including[E, U, L] = original
    }

    /**
     * [[ContravariantBoundedOrder.Including]] implementation delegating to another 
     * [[BoundedOrder.Including]] instance.
     */
    trait Proxy[-E, +L <: E, +U <: E, EE >: E] 
      extends Below.Including.Proxy[E, L, EE]
      with Above.Including.Proxy[E, U, EE]
      with ContravariantBoundedOrder.Including[E, L, U] {

      override protected def original: BoundedOrder.Including[EE, L, U]
    }

    class ProxyImpl[-E, +L <: E, +U <: E, EE >: E](
      override val original: BoundedOrder.Including[EE, L, U]
    ) extends Proxy[E, L, U, EE]
  }

  /**
   * [[ContravariantBoundedOrder]] typeclass received by reverting another 
   * [[ContravariantBoundedOrder]] instance.
   */
  trait Reversed[-E, +L <: E, +U <: E] 
    extends Below.Reversed[E, L] 
    with Above.Reversed[E, U] 
    with ContravariantBoundedOrder[E, L, U]

  class ReversedImpl[-E, +L <: E, +U <: E](original: ContravariantBoundedOrder[E, U, L]) extends Reversed[E, L, U] {

    override val reversed: ContravariantBoundedOrder[E, U, L] = original
  }

  /**
   * [[ContravariantBoundedOrder]] implementation delegating to another 
   * [[BoundedOrder]] instance.
   */
  trait Proxy[-E, +L <: E, +U <: E, EE >: E] 
    extends Below.Proxy[E, L, EE]
    with Above.Proxy[E, U, EE]
    with ContravariantBoundedOrder[E, L, U] {

    override protected def original: BoundedOrder[EE, L, U]
  }

  class ProxyImpl[-E, +L <: E, +U <: E, EE >: E](
    override val original: BoundedOrder[EE, L, U]
  ) extends Proxy[E, L, U, EE]
}