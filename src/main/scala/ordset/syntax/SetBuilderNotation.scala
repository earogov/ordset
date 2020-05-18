package ordset.syntax

import ordset.{Bound, Interval, IntervalMapping}

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

object SetBuilderNotation {

  object x {

    def >[@sp(spNum) E](value: E): Bound.Lower[E] = Bound.Lower(value, isInclusive = false)

    def >=[@sp(spNum) E](value: E): Bound.Lower[E] = Bound.Lower(value, isInclusive = true)

    def <[@sp(spNum) E](value: E): Bound.Upper[E] = Bound.Upper(value, isInclusive = false)

    def <=[@sp(spNum) E](value: E): Bound.Upper[E] = Bound.Upper(value, isInclusive = true)
  }

  implicit def xToUnbounded(value: x.type): Interval.Unbounded.type = Interval.Unbounded

  implicit class ToIntervalMapping[@sp(Boolean) +V](val value: V) {

    def forAll[@sp(spNum) E](interval: Interval[E]): IntervalMapping[E, V] = IntervalMapping(interval, value)
  }

  implicit class ToBoundedInterval[@sp(spNum) E](val left: Bound.Lower[E]) {

    def &(right: Bound.Upper[E]): Interval.Bounded[E] = Interval.Bounded(left, right)
  }
}
