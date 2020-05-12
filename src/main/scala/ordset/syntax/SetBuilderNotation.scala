package ordset.syntax

import ordset.{Bound, Interval, IntervalMapping}

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

object SetBuilderNotation {

  object x {

    def >[@sp(spNum) T](value: T): Bound.Lower[T] = Bound.Lower(value, isInclusive = false)

    def >=[@sp(spNum) T](value: T): Bound.Lower[T] = Bound.Lower(value, isInclusive = true)

    def <[@sp(spNum) T](value: T): Bound.Upper[T] = Bound.Upper(value, isInclusive = false)

    def <=[@sp(spNum) T](value: T): Bound.Upper[T] = Bound.Upper(value, isInclusive = true)
  }

  implicit def xToUnbounded(value: x.type): Interval.Unbounded.type = Interval.Unbounded

  implicit class ToIntervalMapping[@sp(Boolean) +V](val value: V) {

    def forAll[T](interval: Interval[T]): IntervalMapping[T, V] = IntervalMapping(interval, value)
  }

  implicit class ToBoundedInterval[T](val left: Bound.Lower[T]) {

    def &(right: Bound.Upper[T]): Interval.Bounded[T] = Interval.Bounded(left, right)
  }
}
