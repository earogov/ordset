package ordset

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

case class IntervalMapping[@sp(spNum) +T, @sp(Boolean) +V](
    interval: Interval[T], value: V) {

  override def toString: String = s"$value forAll $interval"
}