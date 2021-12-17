package ordset

/**
 * Typeclass specifying set bounded from below and above.
 */ 
trait Bounded[+L, +U] extends Bounded.Below[L] with Bounded.Above[U] with Reversible[Bounded[L, U], Bounded[U, L]] {

  override def reversed: Bounded[U, L] = new Bounded.ReversedImpl(this)
}

object Bounded {

  /**
   * Typeclass specifying set bounded from below.
   */ 
  trait Below[+L] extends Reversible[Below[L], Above[L]] { 

    /** Greatest lower bound of the set. */
    def lowerBound: L

    /** Returns `true`, if lower bound is included in the set. */
    def lowerBoundIncluded: Boolean

    /** Returns `true`, if lower bound is not included in the set. */
    def lowerBoundExcluded: Boolean = !lowerBoundIncluded

    override def reversed: Above[L] = new Above.ReversedImpl(this)
  }

  object Below {

    /**
     * Typeclass specifying set bounded from below. Lower bound is included in the set.
     */ 
    trait Including[+L] extends Below[L] with Reversible[Below.Including[L], Above.Including[L]] { 

      final override val lowerBoundIncluded: Boolean = true

      final override val lowerBoundExcluded: Boolean = false

      override def reversed: Above.Including[L] = new Above.Including.ReversedImpl(this)
    }

    object Including {

      /**
       * [[Bounded.Below.Including]] typeclass received by reverting [[Bounded.Above.Including]] instance.
       */
      trait Reversed[+L] extends Below.Reversed[L] with Below.Including[L]

      class ReversedImpl[+L](original: Above.Including[L]) extends Reversed[L] {

        override val reversed: Above.Including[L] = original
      }
    }

    /**
     * [[Bounded.Below]] typeclass received by reverting [[Bounded.Above]] instance.
     */
    trait Reversed[+L] extends Below[L] {

      override def lowerBound: L = reversed.upperBound

      override def lowerBoundIncluded: Boolean = reversed.upperBoundIncluded
    }

    class ReversedImpl[+L](original: Above[L]) extends Reversed[L] {

      override val reversed: Above[L] = original
    }
  }

  /**
   * Typeclass specifying set bounded from above.
   */ 
  trait Above[+U] extends Reversible[Above[U], Below[U]] { 

    /** Least upper bound of the set. */
    def upperBound: U

    /** Returns `true`, if upper bound is included in the set. */
    def upperBoundIncluded: Boolean

    /** Returns `true`, if upper bound is not included in the set. */
    def upperBoundExcluded: Boolean = !upperBoundIncluded

    override def reversed: Below[U] = new Below.ReversedImpl(this)
  }

  object Above {

    /**
     * Typeclass specifying set bounded from above. Upper bound is included in the set.
     */ 
    trait Including[+U] extends Above[U] with Reversible[Above.Including[U], Below.Including[U]] { 

      final override val upperBoundIncluded: Boolean = true

      final override val upperBoundExcluded: Boolean = false

      override def reversed: Below.Including[U] = new Below.Including.ReversedImpl(this)
    }

    object Including {

      /**
       * [[Bounded.Above.Including]] typeclass received by reverting [[Bounded.Below.Including]] instance.
       */
      trait Reversed[+U] extends Above.Reversed[U] with Above.Including[U]

      class ReversedImpl[+U](original: Below.Including[U]) extends Reversed[U] {

        override val reversed: Below.Including[U] = original
      }
    }

    /**
     * [[Bounded.Above]] typeclass received by reverting [[Bounded.Below]] instance.
     */
    trait Reversed[+U] extends Above[U] {

      override def upperBound: U = reversed.lowerBound

      override def upperBoundIncluded: Boolean = reversed.lowerBoundIncluded
    }

    class ReversedImpl[+U](original: Below[U]) extends Reversed[U] {

      override val reversed: Below[U] = original
    }
  }

  /**
   * Typeclass specifying set bounded from below and above. Lower and upper bounds are included in the set.
   */ 
  trait Including[+L, +U] 
    extends Bounded[L, U]
    with Below.Including[L] 
    with Above.Including[U]
    with Reversible[Including[L, U], Including[U, L]] { 

    override def reversed: Including[U, L] = new Including.ReversedImpl(this)
  }

  object Including {

    /**
     * [[Bounded.Including]] typeclass received by reverting another [[Bounded.Including]] instance.
     */
    trait Reversed[+L, +U] 
      extends Bounded.Reversed[L, U]
      with Below.Including.Reversed[L] 
      with Above.Including.Reversed[U]
      with Including[L, U]

    class ReversedImpl[+L, +U](original: Bounded.Including[U, L]) extends Reversed[L, U] {

      override val reversed: Bounded.Including[U, L] = original
    }
  }

  /**
   * [[Bounded]] typeclass received by reverting another [[Bounded]] instance.
   */
  trait Reversed[+L, +U] extends Below.Reversed[L] with Above.Reversed[U] with Bounded[L, U]

  class ReversedImpl[+L, +U](original: Bounded[U, L]) extends Reversed[L, U] {

    override val reversed: Bounded[U, L] = original
  }
}