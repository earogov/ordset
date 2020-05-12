package ordset.syntax

import ordset.Bound

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

object BoundSyntax {

  implicit class ToBound[@sp(spNum) T](value: T) {

    def `)[`: Bound.Upper[T] = Bound.Upper(value, isInclusive = false)
    def `](`: Bound.Upper[T] = Bound.Upper(value, isInclusive = true)

    def `)`: Bound.Upper[T] = Bound.Upper(value, isInclusive = false)
    def `]`: Bound.Upper[T] = Bound.Upper(value, isInclusive = true)

    def `(`: Bound.Lower[T] = Bound.Lower(value, isInclusive = false)
    def `[`: Bound.Lower[T] = Bound.Lower(value, isInclusive = true)
  }
}