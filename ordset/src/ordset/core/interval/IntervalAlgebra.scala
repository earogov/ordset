package ordset.core.interval

import ordset.core.Bound
import ordset.core.domain.Domain

trait IntervalAlgebra[E, D <: Domain[E]] {

  /**
   * Returns subset of elements that belongs to both input intervals.
   *
   * Set of intervals with cross operation forms a commutative monoid with universal interval as identity:
   *  
   * `cross(x, universal) == cross(universal, x) == x`
   * 
   * {{{
   * 
   *         |-------------|  - x
   *
   *    |-----------|         - y
   * 
   *         |------|         - output
  * }}}
   */
  def cross(x: Interval[E, D], y: Interval[E, D]): Interval[E, D]

  /**
   * Returns subset of elements that belongs to interval and not less than specified bound.
   * {{{
   *
   *              bound
   *                |
   *         |-------------|  - x
   *
   *                |------|  - output
   *              bound
   * }}}
   */
  def takeAbove(bound: Bound.Lower[E], x: Interval[E, D]): Interval[E, D]

  /**
   * Returns subset of elements that belongs to interval and not greater than specified bound.
   * {{{
   *
   *              bound
   *                |
   *         |-------------|  - x
   *
   *         |------|         - output
   *              bound
   * }}}
   */
  def takeBelow(bound: Bound.Upper[E], x: Interval[E, D]): Interval[E, D]
}

object IntervalAlgebra {
  
  implicit def defaultAlgebra[E, D <: Domain[E]](implicit domain: D): IntervalAlgebra[E, D] =
    new DefaultAlgebra(domain, IntervalFactory.defaultFactory(domain))

  final class DefaultAlgebra[E, D <: Domain[E]](
    val domain: D,
    val interval: IntervalFactory[E, D]
  ) extends IntervalAlgebra[E, D] {

    override def cross(x: Interval[E, D], y: Interval[E, D]): Interval[E, D] = x match {
      case x: Interval.Empty[e, d] => x
      case _: Interval.Unbounded[e, d] => y
      case x: Interval.Greater[e, d] => y match {
        case y: Interval.Empty[e, d] => y
        case _: Interval.Unbounded[e, d] => x
        case y: Interval.Greater[e, d] =>
          // x:      |--------------
          // y:  |------------------
          if (domain.boundOrd.lt(y.lower, x.lower)) x
          // x:      |--------------
          // y:          |----------
          else y
        case y: Interval.BoundedAbove[e, d] =>
          // x:      |--------------
          // y: ?--|
          if (domain.boundOrd.lt(y.upper, x.lower)) interval.empty
          else y match {
            // x:      |------------
            // y: ---------|
            case y: Interval.Less[e, d] => interval.betweenBounds(x.lower, y.upper)
            case y: Interval.Between[e, d] =>
              // x:      |----------
              // y:    |---|
              if (domain.boundOrd.lt(y.lower, x.lower)) interval.betweenBounds(x.lower, y.upper)
              // x:      |----------
              // y:        |---|
              else y
          }
      }
      case x: Interval.Less[e, d] => y match {
        case y: Interval.Empty[e, d] => y
        case _: Interval.Unbounded[e, d] => x
        case y: Interval.Less[e, d] =>
          // x: ---------|
          // y: --------------|
          if (domain.boundOrd.lt(x.upper, y.upper)) x
          // x: ---------|
          // y: -----|
          else y
        case y: Interval.BoundedBelow[e, d] =>
          // x: ---------|
          // y:            |------?
          if (domain.boundOrd.lt(x.upper, y.lower)) interval.empty
          else y match {
            // x: ---------|
            // y:      |-----------
            case y: Interval.Greater[e, d] => interval.betweenBounds(y.lower, x.upper)
            case y: Interval.Between[e, d] =>
              // x: ---------|
              // y:        |---|
              if (domain.boundOrd.lt(x.upper, y.upper)) interval.betweenBounds(y.lower, x.upper)
              // x: ---------|
              // y:    |---|
              else y
          }
      }
      case x: Interval.Between[e, d] => y match {
        case y: Interval.Empty[e, d] => y
        case _: Interval.Unbounded[e, d] => x
        case y: Interval.Greater[e, d] =>
          // x:       |-----|
          // y:   |----------------
          if (domain.boundOrd.lteqv(y.lower, x.lower)) x
          // x:       |-----|
          // y:          |---------
          else if (domain.boundOrd.lteqv(y.lower, x.upper)) interval.betweenBounds(y.lower, x.upper)
          // x:       |-----|
          // y:                |---
          else interval.empty
        case y: Interval.Less[e, d] =>
          // x:       |-----|
          // y: ----------------|
          if (domain.boundOrd.lteqv(x.upper, y.upper)) x
          // x:       |-----|
          // y: ---------|
          else if (domain.boundOrd.lteqv(x.lower, y.upper)) interval.betweenBounds(x.lower, y.upper)
          // x:       |-----|
          // y: ----|
          else interval.empty
        case y: Interval.Between[e, d] =>
          if (domain.boundOrd.lteqv(x.lower, y.upper))
            if (domain.boundOrd.lteqv(y.lower, x.lower))
              // x:         |-----|
              // y:      |-----|
              if (domain.boundOrd.lt(y.upper, x.upper)) interval.betweenBounds(x.lower, y.upper)
              // x:         |-----|
              // y:      |-----------|
              else x
            else if (domain.boundOrd.lteqv(y.lower, x.upper))
              // x:         |-----|
              // y:            |-----|
              if (domain.boundOrd.lt(x.upper, y.upper)) interval.betweenBounds(y.lower, x.upper)
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
      case _: Interval.Unbounded[e, d] => interval.aboveBound(bound)
      case x: Interval.Greater[e, d] =>
        //       bound
        // y:      |--------------
        // x:  |------------------
        if (domain.boundOrd.lt(x.lower, bound)) interval.aboveBound(bound)
        //       bound
        // y:      |--------------
        // x:          |----------
        else x
      case x: Interval.BoundedAbove[e, d] =>
        //       bound
        // y:      |--------------
        // x: ?--|
        if (domain.boundOrd.lt(x.upper, bound)) interval.empty
        else x match {
          //       bound
          // y:      |------------
          // x: ---------|
          case x: Interval.Less[e, d] => interval.betweenBounds(bound, x.upper)
          case x: Interval.Between[e, d] =>
            //       bound
            // y:      |----------
            // x:    |---|
            if (domain.boundOrd.lt(x.lower, bound)) interval.betweenBounds(bound, x.upper)
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
      case _: Interval.Unbounded[e, d] => interval.belowBound(bound)
      case x: Interval.Less[e, d] =>
        //           bound
        // y: ---------|
        // x: --------------|
        if (domain.boundOrd.lt(bound, x.upper)) interval.belowBound(bound)
        //           bound
        // y: ---------|
        // x: -----|
        else x
      case x: Interval.BoundedBelow[e, d] =>
        //           bound
        // y: ---------|
        // x:            |------?
        if (domain.boundOrd.lt(bound, x.lower)) interval.empty
        else x match {
          //           bound
          // y: ---------|
          // x:      |-----------
          case x: Interval.Greater[e, d] => interval.betweenBounds(x.lower, bound)
          case x: Interval.Between[e, d] =>
            //           bound
            // y: ---------|
            // x:        |---|
            if (domain.boundOrd.lt(bound, x.upper)) interval.betweenBounds(x.lower, bound)
            //           bound
            // y: ---------|
            // x:    |---|
            else x
        }
    }
  }
}