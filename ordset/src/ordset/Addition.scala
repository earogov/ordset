package ordset

import cats.kernel.Comparison

object Addition {
  
  object Order {
    
    /**
     * [[Order]] typeclass received by reversing another [[Order]] instance.
     */
    trait Reversed[E] extends Order[E] {

      def reversed: Order[E]

      override def compare(x: E, y: E): Int = reversed.compare(y, x)

      override def comparison(x: E, y: E): Comparison = reversed.comparison(y, x)

      override def partialCompare(x: E, y: E): Double = reversed.partialCompare(y, x)

      override def min(x: E, y: E): E = reversed.min(y, x)

      override def max(x: E, y: E): E = reversed.max(y, x)

      override def eqv(x: E, y: E): Boolean = reversed.eqv(y, x)

      override def neqv(x: E, y: E): Boolean = reversed.neqv(y, x)

      override def lteqv(x: E, y: E): Boolean = reversed.lteqv(y, x)

      override def lt(x: E, y: E): Boolean = reversed.lt(y, x)

      override def gteqv(x: E, y: E): Boolean = reversed.gteqv(y, x)

      override def gt(x: E, y: E): Boolean = reversed.gt(y, x)

      override def toOrdering: Ordering[E] = reversed.toOrdering
    }
  }
}
