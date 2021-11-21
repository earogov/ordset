package ordset.core.range

import ordset.Order
import ordset.core.range.Range.NonEmpty
import ordset.core.range.Range.Empty

sealed trait Range[+E]

object Range {

  trait Empty extends Range[Nothing]

  object Empty extends Empty

  trait NonEmpty[+E] extends Range[E] {

    def lower: E

    def upper: E
  }

  object NonEmpty {

    def apply[E](lower: E, upper: E): NonEmpty[E] = new DefaultImpl(lower, upper)

    private case class DefaultImpl[+E](
      override val lower: E,
      override val upper: E
    ) extends NonEmpty[E]
  }
}