package test.ordset.core.domain

import ordset.Order
import ordset.core.{Bound, ExtendedBound}

trait BoundSelector[E] {

  /**
   * Returns bound (if possible) between two input bounds.
   *
   * Output bound must satisfy condition:
   * {{{
   *   `lowerBound` < output bound < `upperBound`
   * }}}
   * with ordering according to input `ord`.
   */
  def between(
    lowerBound: ExtendedBound[E],
    upperBound: ExtendedBound[E]
  )(
    implicit ord: Order[ExtendedBound[E]]
  ): Option[Bound[E]]
}

object BoundSelector {

  implicit def intBoundSelector: BoundSelector[Int] = IntBoundSelector

  // Private section ---------------------------------------------------------- //
  private object IntBoundSelector extends BoundSelector[Int] {

    override def between(
      lowerBound: ExtendedBound[Int],
      upperBound: ExtendedBound[Int]
    )(
      implicit ord: Order[ExtendedBound[Int]]
    ): Option[Bound[Int]] =
      (lowerBound, upperBound) match {
        case (lowerBound: Bound[Int], upperBound: Bound[Int]) =>
          if (ord.gteqv(lowerBound, upperBound)) {
            Option.empty
          } else {
            val m = mid(lowerBound.element, upperBound.element)
            LazyList(0, -1, 1)
              .map(shift => m + shift)
              .flatMap(element =>
                LazyList(0, -1, 1).map {
                  case -1 => Bound.Upper.exclusive(element)
                  case 1 => Bound.Lower.exclusive(element)
                  case 0 => Bound.Upper.inclusive(element)
                }
              )
              .filter(bound => ord.gt(bound, lowerBound) && ord.lt(bound, upperBound))
              .headOption
          }
        case (ExtendedBound.BelowAll, upperBound: Bound[Int]) =>
          Option(upperBound.mapElement(_ - 1))
            .filter(bound => ord.lt(bound, upperBound))
        case (lowerBound: Bound[Int], ExtendedBound.AboveAll) =>
          Option(lowerBound.mapElement(_ + 1))
            .filter(bound => ord.gt(bound, lowerBound))
        case (ExtendedBound.BelowAll, ExtendedBound.AboveAll) =>
          Option(Bound.Upper.inclusive(0))
        case _ =>
          Option.empty
      }

    private def mid(x: Int, y: Int): Int =
      x / 2 + y / 2 + (x % 2 + y % 2) / 2
  }
}