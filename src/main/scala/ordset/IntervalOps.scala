package ordset

import ordset.domain.Domain

trait IntervalOps[E, D <: Domain[E]] {

  def domain: D

  def cross(x: Interval[E, D], y: Interval[E, D]): Interval[E, D]
}
