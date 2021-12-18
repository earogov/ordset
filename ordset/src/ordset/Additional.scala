package ordset

import cats.kernel.Comparison

object Additional {
  
  object Order {
    
    /**
     * [[Order]] typeclass received by reversing another [[Order]] instance.
     */
    trait Reversed[E] extends Order[E] { self =>

      def reversed: Order[E]

      final override def compare(x: E, y: E): Int = reversed.compare(y, x)

      final override def comparison(x: E, y: E): Comparison =
        // Implementation of `reversed` order may use type specific operators like `>` and `<` instead of `compare`.
        // In that case will get wrong behavior. We should redefine implementation to guarantee correctness.
        Comparison.fromInt(compare(x, y))

      final override def partialCompare(x: E, y: E): Double = 
        // See comments in `comparison`.
        compare(x, y).toDouble

      final override def min(x: E, y: E): E = 
        // See comments in `comparison`.
        if compare(x, y) <= 0 then x else y

      final override def max(x: E, y: E): E = 
        // See comments in `comparison`.
        if compare(x, y) >= 0 then x else y

      final override def eqv(x: E, y: E): Boolean =
        // See comments in `comparison`.
        compare(x, y) == 0

      final override def neqv(x: E, y: E): Boolean = 
        // See comments in `comparison`.
        compare(x, y) != 0

      final override def lteqv(x: E, y: E): Boolean =
        // See comments in `comparison`.
        compare(x, y) <= 0

      final override def lt(x: E, y: E): Boolean = 
        // See comments in `comparison`.
        compare(x, y) < 0

      final override def gteqv(x: E, y: E): Boolean =
        // See comments in `comparison`.
        compare(x, y) >= 0

      final override def gt(x: E, y: E): Boolean = 
        // See comments in `comparison`.
        compare(x, y) > 0

      final override def toOrdering: Ordering[E] = 
        // See comments in `comparison`.
        Ordering.fromLessThan(lt)
    }
  }
}
