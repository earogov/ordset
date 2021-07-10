package ordset.core.syntax

import ordset.core.Bound

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

object BoundSyntax {

  implicit class ToBound[@sp(spNum) E](element: E) {

    def `)[`: Bound.Upper[E] = Bound.Upper(element, isInclusive = false)
    def `](`: Bound.Upper[E] = Bound.Upper(element, isInclusive = true)

    def `)`: Bound.Upper[E] = Bound.Upper(element, isInclusive = false)
    def `]`: Bound.Upper[E] = Bound.Upper(element, isInclusive = true)

    def `(`: Bound.Lower[E] = Bound.Lower(element, isInclusive = false)
    def `[`: Bound.Lower[E] = Bound.Lower(element, isInclusive = true)
  }
}