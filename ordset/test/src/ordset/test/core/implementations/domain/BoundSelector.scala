package ordset.test.core.implementations.domain

import ordset.Order
import ordset.core.{Bound, ExtendedBound}

trait BoundSelector[E] {

  /**
   * Returns bound (if possible) between two input bounds.
   *
   * Output bound must satisfy condition:
   * {{{
   *   `lower` < output bound < `upper`
   * }}}
   * with ordering according to input `ord`.
   */
  def between(
    lower: ExtendedBound[E],
    upper: ExtendedBound[E]
  )(
    implicit ord: Order[ExtendedBound[E]]
  ): Option[Bound[E]]
}

object BoundSelector {

  implicit def intBoundSelector: BoundSelector[Int] = IntBoundSelector

  // Private section ---------------------------------------------------------- //
  private object IntBoundSelector extends BoundSelector[Int] {

    override def between(
      lower: ExtendedBound[Int],
      upper: ExtendedBound[Int]
    )(
      implicit ord: Order[ExtendedBound[Int]]
    ): Option[Bound[Int]] =
      (lower, upper) match {
        case (lower: Bound[Int], upper: Bound[Int]) =>
          if (ord.gteqv(lower, upper)) {
            Option.empty
          } else {
            val m = mid(lower.element, upper.element)
            LazyList(0, -1, 1)
              .map(shift => m + shift)
              .flatMap(element =>
                LazyList(0, -1, 1).map {
                  case -1 => Bound.Upper.excluding(element)
                  case 1 => Bound.Lower.excluding(element)
                  case 0 => Bound.Upper.including(element)
                }
              )
              .filter(bound => ord.gt(bound, lower) && ord.lt(bound, upper))
              .headOption
          }
        case (ExtendedBound.BelowAll, upper: Bound[Int]) =>
          Option(upper.mapElement(_ - 1))
            .filter(bound => ord.lt(bound, upper))
        case (lower: Bound[Int], ExtendedBound.AboveAll) =>
          Option(lower.mapElement(_ + 1))
            .filter(bound => ord.gt(bound, lower))
        case (ExtendedBound.BelowAll, ExtendedBound.AboveAll) =>
          Option(Bound.Upper.including(0))
        case _ =>
          Option.empty
      }

    private def mid(x: Int, y: Int): Int =
      x / 2 + y / 2 + (x % 2 + y % 2) / 2
  }
}