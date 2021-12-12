package ordset

/**
 * Typeclass specifying set bounded from below and above.
 */ 
trait Bounded[+L, +U] extends Bounded.Below[L] with Bounded.Above[U]

object Bounded {

  /**
   * Typeclass specifying set bounded from below.
   */ 
  trait Below[+L] {

    /** Greatest lower bound of the set. */
    def lowerBound: L

    /** Returns `true`, if lower bound is included in the set. */
    def lowerBoundIncluded: Boolean

    /** Returns `true`, if lower bound is not included in the set. */
    def lowerBoundExcluded: Boolean = !lowerBoundIncluded
  }

  object Below {

    /**
     * Typeclass specifying set bounded from below. Lower bound is included in the set.
     */ 
    trait Including[+L] extends Bounded.Below[L] {

      final override val lowerBoundIncluded: Boolean = true

      final override val lowerBoundExcluded: Boolean = false
    }
  }

  /**
   * Typeclass specifying set bounded from above.
   */ 
  trait Above[+U] {

    /** Least upper bound of the set. */
    def upperBound: U

    /** Returns `true`, if upper bound is included in the set. */
    def upperBoundIncluded: Boolean

    /** Returns `true`, if upper bound is not included in the set. */
    def upperBoundExcluded: Boolean
  }

  object Above {

    /**
     * Typeclass specifying set bounded from above. Upper bound is included in the set.
     */ 
    trait Including[+U] extends Bounded.Above[U] {

      final override val upperBoundIncluded: Boolean = true

      final override val upperBoundExcluded: Boolean = false
    }
  }

  /**
   * Typeclass specifying set bounded from below and above. Lower and upper bounds are included in the set.
   */ 
  trait Including[+L, +U] extends Below.Including[L] with Above.Including[U]
}