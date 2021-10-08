package ordset.core.interval

import ordset.core.Bound
import ordset.core.domain.{Domain, DomainOps}

trait IntervalOps[E, D <: Domain[E]] {

  def domainOps: DomainOps[E, D]

  /**
   * Returns subset of elements that belongs to both input intervals.
   *
   * Set of intervals with cross operation form a commutative monoid with universal interval as identity:
   * {{{ cross(x, universal) == cross(universal, x) == x }}}
   */
  def cross(x: Interval[E, D], y: Interval[E, D]): Interval[E, D]

  /**
   * Returns subset of elements that belongs to interval and not less then specified bound.
   * {{{
   *
   *              bound
   *                |
   *         |-------------|  - x
   *
   *                |
   *                V
   *
   *                |------|  - output
   *              bound
   * }}}
   */
  def takeAbove(bound: Bound.Lower[E], x: Interval[E, D]): Interval[E, D] = cross(x, domainOps.interval(bound))

  /**
   * Returns subset of elements that belongs to interval and not greater then specified bound.
   * {{{
   *
   *              bound
   *                |
   *         |-------------|  - x
   *
   *                |
   *                V
   *
   *         |------|         - output
   *              bound
   * }}}
   */
  def takeBelow(bound: Bound.Upper[E], x: Interval[E, D]): Interval[E, D] = cross(x, domainOps.interval(bound))
}

object IntervalOps {
  
  final class UnboundedOps[E, D <: Domain[E]](
    override val domainOps: DomainOps[E, D]
  ) extends IntervalOps[E, D] {

    override def cross(x: Interval[E, D], y: Interval[E, D]): Interval[E, D] = x match {
      case x: Interval.Empty[e, d] => x
      case _: Interval.Universal[e, d] => y
      case x: Interval.Greater[e, d] => y match {
        case y: Interval.Empty[e, d] => y
        case _: Interval.Universal[e, d] => x
        case y: Interval.Greater[e, d] =>
          // x:      |--------------
          // y:  |------------------
          if (domainOps.boundOrd.lt(y.lowerBound, x.lowerBound)) x
          // x:      |--------------
          // y:          |----------
          else y
        case y: Interval.WithUpperBound[e, d] =>
          // x:      |--------------
          // y: ?--|
          if (domainOps.boundOrd.lt(y.upperBound, x.lowerBound)) domainOps.interval.empty
          else y match {
            // x:      |------------
            // y: ---------|
            case y: Interval.Less[e, d] => domainOps.interval(x.lowerBound, y.upperBound)
            case y: Interval.Between[e, d] =>
              // x:      |----------
              // y:    |---|
              if (domainOps.boundOrd.lt(y.lowerBound, x.lowerBound)) domainOps.interval(x.lowerBound, y.upperBound)
              // x:      |----------
              // y:        |---|
              else y
          }
      }
      case x: Interval.Less[e, d] => y match {
        case y: Interval.Empty[e, d] => y
        case _: Interval.Universal[e, d] => x
        case y: Interval.Less[e, d] =>
          // x: ---------|
          // y: --------------|
          if (domainOps.boundOrd.lt(x.upperBound, y.upperBound)) x
          // x: ---------|
          // y: -----|
          else y
        case y: Interval.WithLowerBound[e, d] =>
          // x: ---------|
          // y:            |------?
          if (domainOps.boundOrd.lt(x.upperBound, y.lowerBound)) domainOps.interval.empty
          else y match {
            // x: ---------|
            // y:      |-----------
            case y: Interval.Greater[e, d] => domainOps.interval(y.lowerBound, x.upperBound)
            case y: Interval.Between[e, d] =>
              // x: ---------|
              // y:        |---|
              if (domainOps.boundOrd.lt(x.upperBound, y.upperBound)) domainOps.interval(y.lowerBound, x.upperBound)
              // x: ---------|
              // y:    |---|
              else y
          }
      }
      case x: Interval.Between[e, d] => y match {
        case y: Interval.Empty[e, d] => y
        case _: Interval.Universal[e, d] => x
        case y: Interval.Greater[e, d] =>
          // x:       |-----|
          // y:   |----------------
          if (domainOps.boundOrd.lteqv(y.lowerBound, x.lowerBound)) x
          // x:       |-----|
          // y:          |---------
          else if (domainOps.boundOrd.lteqv(y.lowerBound, x.upperBound)) domainOps.interval(y.lowerBound, x.upperBound)
          // x:       |-----|
          // y:                |---
          else domainOps.interval.empty
        case y: Interval.Less[e, d] =>
          // x:       |-----|
          // y: ----------------|
          if (domainOps.boundOrd.lteqv(x.upperBound, y.upperBound)) x
          // x:       |-----|
          // y: ---------|
          else if (domainOps.boundOrd.lteqv(x.lowerBound, y.upperBound)) domainOps.interval(x.lowerBound, y.upperBound)
          // x:       |-----|
          // y: ----|
          else domainOps.interval.empty
        case y: Interval.Between[e, d] =>
          if (domainOps.boundOrd.lteqv(x.lowerBound, y.upperBound))
            if (domainOps.boundOrd.lteqv(y.lowerBound, x.lowerBound))
              // x:         |-----|
              // y:      |-----|
              if (domainOps.boundOrd.lt(y.upperBound, x.upperBound)) domainOps.interval(x.lowerBound, y.upperBound)
              // x:         |-----|
              // y:      |-----------|
              else x
            else if (domainOps.boundOrd.lteqv(y.lowerBound, x.upperBound))
              // x:         |-----|
              // y:            |-----|
              if (domainOps.boundOrd.lt(x.upperBound, y.upperBound)) domainOps.interval(y.lowerBound, x.upperBound)
              // x:         |-----|
              // y:           |-|
              else y
            // x:         |-----|
            // y:                 |-----|
            else domainOps.interval.empty
          // x:              |-----|
          // y:      |-----|
          else domainOps.interval.empty
      }
    }

    override def takeAbove(bound: Bound.Lower[E], x: Interval[E, D]): Interval[E, D] = x match {
      // takeAbove(x, bound) = cross(x, y) where
      // y = domainOps.interval(bound)
      case x: Interval.Empty[e, d] => x
      case _: Interval.Universal[e, d] => domainOps.interval(bound)
      case x: Interval.Greater[e, d] =>
        //       bound
        // y:      |--------------
        // x:  |------------------
        if (domainOps.boundOrd.lt(x.lowerBound, bound)) domainOps.interval(bound)
        //       bound
        // y:      |--------------
        // x:          |----------
        else x
      case x: Interval.WithUpperBound[e, d] =>
        //       bound
        // y:      |--------------
        // x: ?--|
        if (domainOps.boundOrd.lt(x.upperBound, bound)) domainOps.interval.empty
        else x match {
          //       bound
          // y:      |------------
          // x: ---------|
          case x: Interval.Less[e, d] => domainOps.interval(bound, x.upperBound)
          case x: Interval.Between[e, d] =>
            //       bound
            // y:      |----------
            // x:    |---|
            if (domainOps.boundOrd.lt(x.lowerBound, bound)) domainOps.interval(bound, x.upperBound)
            //       bound
            // y:      |----------
            // x:        |---|
            else x
        }
    }

    override def takeBelow(bound: Bound.Upper[E], x: Interval[E, D]): Interval[E, D] = x match {
      // takeBelow(x, bound) = cross(x, y) where
      // y = domainOps.interval(bound)
      case x: Interval.Empty[e, d] => x
      case _: Interval.Universal[e, d] => domainOps.interval(bound)
      case x: Interval.Less[e, d] =>
        //           bound
        // y: ---------|
        // x: --------------|
        if (domainOps.boundOrd.lt(bound, x.upperBound)) domainOps.interval(bound)
        //           bound
        // y: ---------|
        // x: -----|
        else x
      case x: Interval.WithLowerBound[e, d] =>
        //           bound
        // y: ---------|
        // x:            |------?
        if (domainOps.boundOrd.lt(bound, x.lowerBound)) domainOps.interval.empty
        else x match {
          //           bound
          // y: ---------|
          // x:      |-----------
          case x: Interval.Greater[e, d] => domainOps.interval(x.lowerBound, bound)
          case x: Interval.Between[e, d] =>
            //           bound
            // y: ---------|
            // x:        |---|
            if (domainOps.boundOrd.lt(bound, x.upperBound)) domainOps.interval(x.lowerBound, bound)
            //           bound
            // y: ---------|
            // x:    |---|
            else x
        }
    }
  }
}