package ordset.core.interval

import ordset.core.Bound
import ordset.core.domain.Domain

trait IntervalOps[E, D <: Domain[E]] {

  /**
   * Returns subset of elements that belongs to both input intervals.
   *
   * Set of intervals with cross operation forms a commutative monoid with universal interval as identity:
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
  def takeAbove(bound: Bound.Lower[E], x: Interval[E, D]): Interval[E, D]

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
  def takeBelow(bound: Bound.Upper[E], x: Interval[E, D]): Interval[E, D]
}

object IntervalOps {
  
  implicit def defaultOps[E, D <: Domain[E]](
    implicit 
    domain: D,
    intervalBuilder: IntervalBuilder[E, D]
  ): IntervalOps[E, D] =
    domain match {
      case d: Domain.Unbounded[E] => new UnboundedOps(d, intervalBuilder)
      // TODO: implement bounded builder
      case d: Domain.Bounded[E] => ???
    }

  final class UnboundedOps[E, D <: Domain[E]](
    val domain: D & Domain.Unbounded[E],
    val interval: IntervalBuilder[E, D]
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
          if (domain.boundOrd.lt(y.lowerBound, x.lowerBound)) x
          // x:      |--------------
          // y:          |----------
          else y
        case y: Interval.WithUpperBound[e, d] =>
          // x:      |--------------
          // y: ?--|
          if (domain.boundOrd.lt(y.upperBound, x.lowerBound)) interval.empty
          else y match {
            // x:      |------------
            // y: ---------|
            case y: Interval.Less[e, d] => interval.betweenBounds(x.lowerBound, y.upperBound)
            case y: Interval.Between[e, d] =>
              // x:      |----------
              // y:    |---|
              if (domain.boundOrd.lt(y.lowerBound, x.lowerBound)) interval.betweenBounds(x.lowerBound, y.upperBound)
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
          if (domain.boundOrd.lt(x.upperBound, y.upperBound)) x
          // x: ---------|
          // y: -----|
          else y
        case y: Interval.WithLowerBound[e, d] =>
          // x: ---------|
          // y:            |------?
          if (domain.boundOrd.lt(x.upperBound, y.lowerBound)) interval.empty
          else y match {
            // x: ---------|
            // y:      |-----------
            case y: Interval.Greater[e, d] => interval.betweenBounds(y.lowerBound, x.upperBound)
            case y: Interval.Between[e, d] =>
              // x: ---------|
              // y:        |---|
              if (domain.boundOrd.lt(x.upperBound, y.upperBound)) interval.betweenBounds(y.lowerBound, x.upperBound)
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
          if (domain.boundOrd.lteqv(y.lowerBound, x.lowerBound)) x
          // x:       |-----|
          // y:          |---------
          else if (domain.boundOrd.lteqv(y.lowerBound, x.upperBound)) interval.betweenBounds(y.lowerBound, x.upperBound)
          // x:       |-----|
          // y:                |---
          else interval.empty
        case y: Interval.Less[e, d] =>
          // x:       |-----|
          // y: ----------------|
          if (domain.boundOrd.lteqv(x.upperBound, y.upperBound)) x
          // x:       |-----|
          // y: ---------|
          else if (domain.boundOrd.lteqv(x.lowerBound, y.upperBound)) interval.betweenBounds(x.lowerBound, y.upperBound)
          // x:       |-----|
          // y: ----|
          else interval.empty
        case y: Interval.Between[e, d] =>
          if (domain.boundOrd.lteqv(x.lowerBound, y.upperBound))
            if (domain.boundOrd.lteqv(y.lowerBound, x.lowerBound))
              // x:         |-----|
              // y:      |-----|
              if (domain.boundOrd.lt(y.upperBound, x.upperBound)) interval.betweenBounds(x.lowerBound, y.upperBound)
              // x:         |-----|
              // y:      |-----------|
              else x
            else if (domain.boundOrd.lteqv(y.lowerBound, x.upperBound))
              // x:         |-----|
              // y:            |-----|
              if (domain.boundOrd.lt(x.upperBound, y.upperBound)) interval.betweenBounds(y.lowerBound, x.upperBound)
              // x:         |-----|
              // y:           |-|
              else y
            // x:         |-----|
            // y:                 |-----|
            else interval.empty
          // x:              |-----|
          // y:      |-----|
          else interval.empty
      }
    }

    override def takeAbove(bound: Bound.Lower[E], x: Interval[E, D]): Interval[E, D] = x match {
      // takeAbove(x, bound) = cross(x, y) where
      // y = interval(bound)
      case x: Interval.Empty[e, d] => x
      case _: Interval.Universal[e, d] => interval.aboveBound(bound)
      case x: Interval.Greater[e, d] =>
        //       bound
        // y:      |--------------
        // x:  |------------------
        if (domain.boundOrd.lt(x.lowerBound, bound)) interval.aboveBound(bound)
        //       bound
        // y:      |--------------
        // x:          |----------
        else x
      case x: Interval.WithUpperBound[e, d] =>
        //       bound
        // y:      |--------------
        // x: ?--|
        if (domain.boundOrd.lt(x.upperBound, bound)) interval.empty
        else x match {
          //       bound
          // y:      |------------
          // x: ---------|
          case x: Interval.Less[e, d] => interval.betweenBounds(bound, x.upperBound)
          case x: Interval.Between[e, d] =>
            //       bound
            // y:      |----------
            // x:    |---|
            if (domain.boundOrd.lt(x.lowerBound, bound)) interval.betweenBounds(bound, x.upperBound)
            //       bound
            // y:      |----------
            // x:        |---|
            else x
        }
    }

    override def takeBelow(bound: Bound.Upper[E], x: Interval[E, D]): Interval[E, D] = x match {
      // takeBelow(x, bound) = cross(x, y) where
      // y = interval(bound)
      case x: Interval.Empty[e, d] => x
      case _: Interval.Universal[e, d] => interval.belowBound(bound)
      case x: Interval.Less[e, d] =>
        //           bound
        // y: ---------|
        // x: --------------|
        if (domain.boundOrd.lt(bound, x.upperBound)) interval.belowBound(bound)
        //           bound
        // y: ---------|
        // x: -----|
        else x
      case x: Interval.WithLowerBound[e, d] =>
        //           bound
        // y: ---------|
        // x:            |------?
        if (domain.boundOrd.lt(bound, x.lowerBound)) interval.empty
        else x match {
          //           bound
          // y: ---------|
          // x:      |-----------
          case x: Interval.Greater[e, d] => interval.betweenBounds(x.lowerBound, bound)
          case x: Interval.Between[e, d] =>
            //           bound
            // y: ---------|
            // x:        |---|
            if (domain.boundOrd.lt(bound, x.upperBound)) interval.betweenBounds(x.lowerBound, bound)
            //           bound
            // y: ---------|
            // x:    |---|
            else x
        }
    }
  }
}