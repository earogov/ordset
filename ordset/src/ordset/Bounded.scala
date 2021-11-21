package ordset

/**
 * Typeclass specifying set bounded from below and above.
 */ 
trait Bounded[+E] extends Bounded.Below[E] with Bounded.Above[E]

object Bounded {

  /**
   * Typeclass specifying set bounded from below.
   */ 
  trait Below[+E] {

    /** Greatest lower bound of the set. */
    def lowerBound: E

    /** Returns `true`, if lower bound is included in the set. */
    def lowerBoundIncluded: Boolean

    /** Returns `true`, if lower bound is not included in the set. */
    def lowerBoundExcluded: Boolean = !lowerBoundIncluded
  }

  object Below {

    /**
     * Typeclass specifying set bounded from below. Lower bound is included in the set.
     */ 
    trait Including[+E] extends Bounded.Below[E] {

      final override val lowerBoundIncluded: Boolean = true

      final override val lowerBoundExcluded: Boolean = false
    }
  }

  /**
   * Typeclass specifying set bounded from above.
   */ 
  trait Above[+E] {

    /** Least upper bound of the set. */
    def upperBound: E

    /** Returns `true`, if upper bound is included in the set. */
    def upperBoundIncluded: Boolean

    /** Returns `true`, if upper bound is not included in the set. */
    def upperBoundExcluded: Boolean
  }

  object Above {

    /**
     * Typeclass specifying set bounded from above. Upper bound is included in the set.
     */ 
    trait Including[+E] extends Bounded.Above[E] {

      final override val upperBoundIncluded: Boolean = true

      final override val upperBoundExcluded: Boolean = false
    }
  }

  /**
   * Typeclass specifying set bounded from below and above. Lower and upper bounds are included in the set.
   */ 
  trait Including[+E] extends Below.Including[E] with Above.Including[E]
}