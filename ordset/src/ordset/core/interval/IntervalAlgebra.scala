package ordset.core.interval

import ordset.core.Bound
import ordset.core.domain.Domain

trait IntervalAlgebra[E, D[X] <: Domain[X]] {

  /** 
   * Domain of ordered elements. 
   */
  def domain: D[E]

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
  def takeAboveBound(bound: Bound.Lower[E], x: Interval[E, D]): Interval[E, D]

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
  def takeBelowBound(bound: Bound.Upper[E], x: Interval[E, D]): Interval[E, D]
}

object IntervalAlgebra {
  
  implicit def defaultAlgebra[E, D[X] <: Domain[X]](implicit domain: D[E]): IntervalAlgebra[E, D] =
    new DefaultAlgebra(domain, IntervalFactory.defaultFactory(domain))

  class DefaultAlgebra[E, D[X] <: Domain[X]](
    override val domain: D[E],
    val interval: IntervalFactory[E, D]
  ) extends IntervalAlgebra[E, D] {

    override def cross(x: Interval[E, D], y: Interval[E, D]): Interval[E, D] = {
      val ord = domain.boundOrd
      x match {
        case x: Interval.Empty[e, d]=> x
        case _: Interval.Unbounded[e, d] => y
        case x: Interval.Greater[e, d] => takeAboveBound(x.lower, y)
        case x: Interval.Less[e, d] => takeBelowBound(x.upper, y)
        case x: Interval.Between[e, d] => y match {
          case y: Interval.Empty[e, d] => y
          case _: Interval.Unbounded[e, d] => x
          case y: Interval.Greater[e, d] => takeAboveBoundOfBoundedInterval(y.lower, x)
          case y: Interval.Less[e, d] => takeBelowBoundOfBoundedInterval(y.upper, x)
          case y: Interval.Between[e, d] =>
            if ord.lteqv(x.lower, y.upper) then
              if ord.lteqv(y.lower, x.lower) then
                // x:         |-----|
                // y:      |-----|
                if ord.lt(y.upper, x.upper) then interval.betweenBounds(x.lower, y.upper)
                // x:         |-----|
                // y:      |-----------|
                else x
              else if ord.lteqv(y.lower, x.upper) then
                // x:         |-----|
                // y:            |-----|
                if ord.lt(x.upper, y.upper) then interval.betweenBounds(y.lower, x.upper)
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
    }

    override def takeAboveBound(bound: Bound.Lower[E], x: Interval[E, D]): Interval[E, D] = {
      val ord = domain.boundOrd
      x match {
        case x: Interval.Empty[e, d] => x
        case _: Interval.Unbounded[e, d] => interval.aboveBound(bound)
        case x: Interval.Between[e, d] => takeAboveBoundOfBoundedInterval(bound, x)
        case x: Interval.Greater[e, d] =>
          //       bound
          // y:      |--------------
          // x:  |------------------
          if ord.lt(x.lower, bound) then interval.aboveBound(bound)
          //       bound
          // y:      |--------------
          // x:          |----------
          else x
        case x: Interval.Less[e, d] =>
          //       bound
          // y:      |--------------
          // x: --|
          if ord.lt(x.upper, bound) then interval.empty
          //       bound
          // y:      |--------------
          // x: ------------|
          else interval.betweenBounds(bound, x.upper)
      }
    }

    override def takeBelowBound(bound: Bound.Upper[E], x: Interval[E, D]): Interval[E, D] = {
      val ord = domain.boundOrd
      x match {
        case x: Interval.Empty[e, d] => x
        case _: Interval.Unbounded[e, d] => interval.belowBound(bound)
        case x: Interval.Between[e, d] => takeBelowBoundOfBoundedInterval(bound, x)
        case x: Interval.Less[e, d] =>
          //           bound
          // y: ---------|
          // x: --------------|
          if ord.lt(bound, x.upper) then interval.belowBound(bound)
          //           bound
          // y: ---------|
          // x: -----|
          else x
        case x: Interval.Greater[e, d] =>
          //           bound
          // y: ---------|
          // x:            |----
          if ord.lt(bound, x.lower) then interval.empty
          //           bound
          // y: ---------|
          // x:      |----------
          else interval.betweenBounds(x.lower, bound)
      }
    }

    private def takeAboveBoundOfBoundedInterval(bound: Bound.Lower[E], x: Interval.Between[E, D]): Interval[E, D] = {
      val ord = domain.boundOrd
      //       bound
      // y:      |----------
      // x: |---|
      if ord.lt(x.upper, bound) then interval.empty
      //       bound
      // y:      |----------
      // x:    |---|
      else if ord.lt(x.lower, bound) then interval.betweenBounds(bound, x.upper)
      //       bound
      // y:      |----------
      // x:         |---|
      else x
    }

    private def takeBelowBoundOfBoundedInterval(bound: Bound.Upper[E], x: Interval.Between[E, D]): Interval[E, D] = {
      val ord = domain.boundOrd
      //           bound
      // y: ---------|
      // x:            |---|
      if ord.lt(bound, x.lower) then interval.empty
      //           bound
      // y: ---------|
      // x:        |---|
      else if ord.lt(bound, x.upper) then interval.betweenBounds(x.lower, bound)
      //           bound
      // y: ---------|
      // x:    |---|
      else x
    }
  }
}