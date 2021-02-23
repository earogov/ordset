package ordset.core.syntax

import ordset.core.Bound

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

object BoundSyntax {

  implicit class ToBound[@sp(spNum) E](value: E) {

    def `)[`: Bound.Upper[E] = Bound.Upper(value, isInclusive = false)
    def `](`: Bound.Upper[E] = Bound.Upper(value, isInclusive = true)

    def `)`: Bound.Upper[E] = Bound.Upper(value, isInclusive = false)
    def `]`: Bound.Upper[E] = Bound.Upper(value, isInclusive = true)

    def `(`: Bound.Lower[E] = Bound.Lower(value, isInclusive = false)
    def `[`: Bound.Lower[E] = Bound.Lower(value, isInclusive = true)
  }
}