package ordset.core.syntax

import ordset.core.{Bound, ExtendedBound}

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

object BoundSyntax {

  val AboveAll: ExtendedBound.AboveAll.type = ExtendedBound.AboveAll
  
  val BelowAll: ExtendedBound.BelowAll.type = ExtendedBound.BelowAll
  
  implicit class ToBound[@sp(spNum) E](element: E) {

    def `)[`: Bound.Upper[E] = Bound.Upper(element, isIncluding = false)
    def `](`: Bound.Upper[E] = Bound.Upper(element, isIncluding = true)

    def `)`: Bound.Upper[E] = Bound.Upper(element, isIncluding = false)
    def `]`: Bound.Upper[E] = Bound.Upper(element, isIncluding = true)

    def `(`: Bound.Lower[E] = Bound.Lower(element, isIncluding = false)
    def `[`: Bound.Lower[E] = Bound.Lower(element, isIncluding = true)
  }
}