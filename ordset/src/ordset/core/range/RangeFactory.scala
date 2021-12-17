package ordset.core.range

import ordset.{Order, BoundedOrder}

/**
 * Typeclass with capability to construct ranges.
 * 
 * @tparam E type of range elements.
 * @tparam R type of range.
 */
trait RangeFactory[E, +R <: Range[E]] {

  /**
   * Order typeclass for elements.
   */
  def order: Order[E]

  /** 
   * Returns empty range.
   */
  def empty: R & Range.Empty

  /** 
   * Returns range of elements between `lower` (including) and `upper` (including).
   * 
   * Returns [[Range.Empty]], if there is no element `e` in domain that satisfies condition:
   * 
   * `lower ≤ e ≤ upper`
   */
  def between(lower: E, upper: E): R

  /** 
   * Returns range of elements between lower bound of range `r` (including) and upper bound of `r` (including).
   * 
   * Returns [[Range.Empty]], if there is no element `e` in domain that satisfies condition:
   * 
   * `r.lower ≤ e ≤ r.upper`
   */
  def like(r: Range[E]): R = 
    r match {
      case r: Range.NonEmpty[E] => likeNE(r)
      case _ => empty
    }

  /** 
   * Returns range of elements between lower bound of range `r` (including) and upper bound of `r` (including).
   * 
   * Returns [[Range.Empty]], if there is no element `e` in domain that satisfies condition:
   * 
   * `r.lower ≤ e ≤ r.upper`
   */
  def likeNE(r: Range.NonEmpty[E]): R = between(r.lower, r.upper)
}

object RangeFactory {

  /**
   * Typeclass with capability to construct ranges on unbounded domain.
   * 
   * @tparam E type of range elements.
   * @tparam R type of range.
   */
  trait Unbounded[E, +R <: Range[E]] extends RangeFactory[E, R] {

    override def between(lower: E, upper: E): R =
      if order.lteqv(lower, upper) then cons(lower, upper)
      else empty

    /**
     * Returns range of elements between `lower` (including) and `upper` (including).
     * 
     * Preconditions:
     * <tr>1. `lower ≤ upper`</tr>
     */
    protected def cons(lower: E, upper: E): R & Range.NonEmpty[E]
  }

  /**
   * Typeclass with capability to construct ranges on bounded from below domain.
   * 
   * @tparam E type of range elements.
   * @tparam R type of range.
   */
  trait BoundedBelow[E, +R <: Range[E]] extends RangeFactory[E, R] {
  
    override def order: BoundedOrder.Below[E, E]

    override def between(lower: E, upper: E): R = {
      val lb = order.lowerBound
      if order.lteqv(lower, upper) then
        //               lower  upper
        // range:          |-----|
        //             lb
        // bounds:      |-----------------
        if order.lteqv(lb, lower) then cons(lower,  upper)
        //               lower  upper
        // range:          |-----|
        //                   lb
        // bounds:            |----------
        else if order.lteqv(lb, upper) then cons(lb, upper)
        //               lower  upper
        // range:          |-----|
        //                         lb
        // bounds:                  |----
        else empty
      else empty
    }

    /**
     * Returns range of elements between `lower` (including) and `upper` (including).
     * 
     * Preconditions:
     * <tr>1. `lower ≤ upper`</tr>
     * <tr>2. `order.lowerBound ≤ lower`</tr>
     * <tr>3. `order.lowerBound ≤ upper`</tr>
     */
    protected def cons(lower: E, upper: E): R & Range.NonEmpty[E]
  }

  /**
   * Typeclass with capability to construct ranges on bounded from above domain.
   * 
   * @tparam E type of range elements.
   * @tparam R type of range.
   */
  trait BoundedAbove[E, +R <: Range[E]] extends RangeFactory[E, R] {

    override def order: BoundedOrder.Above[E, E]

    override def between(lower: E, upper: E): R = {
      val ub = order.upperBound
      if order.lteqv(lower, upper) then
        //              lower   upper
        // range:          |-----|
        //                            ub
        // bounds:      --------------|
        if order.gteqv(ub, upper) then cons(lower, upper)
        //              lower   upper
        // range:          |-----|
        //                    ub
        // bounds:     -------|
        else if order.gteqv(ub, lower) then cons(lower, ub)
        //              lower   upper
        // range:          |-----|
        //               ub
        // bounds:    ---|
        else empty
      else empty
    }

    /**
     * Returns range of elements between `lower` (including) and `upper` (including).
     * 
     * Preconditions:
     * <tr>1. `lower ≤ upper`</tr>
     * <tr>2. `lower ≤ order.upperBound`</tr>
     * <tr>3. `upper ≤ order.upperBound`</tr>
     */
    protected def cons(lower: E, upper: E): R & Range.NonEmpty[E]
  }

  /**
   * Typeclass with capability to construct ranges on bounded domain.
   * 
   * @tparam E type of range elements.
   * @tparam R type of range.
   */
  trait Bounded[E, +R <: Range[E]] extends RangeFactory[E, R] {

    override def order: BoundedOrder[E, E, E]

    override def between(lower: E, upper: E): R = {
      val lb = order.lowerBound
      val ub = order.upperBound
      if order.lteqv(lower, upper) then
        if order.lteqv(lower, ub) then
          if order.lteqv(lb, lower) then
            //              lower   upper
            // range:          |-----|
            //             lb     ub
            // bounds:      |-----|
            if order.lt(ub, upper) then cons(lower, ub)
            //              lower   upper
            // range:          |-----|
            //             lb           ub
            // bounds:      |-----------|
            else cons(lower, upper)
          else if order.lteqv(lb, upper) then
            //              lower   upper
            // range:          |-----|
            //                   lb     ub
            // bounds:            |-----|
            if order.lt(upper, ub) then cons(lb, upper)
            //              lower   upper 
            // range:          |-----|
            //                  lb  ub
            // bounds:           |-|
            else cons(lb, ub)
          //              lower   upper
          // range:          |-----|
          //                        lb     ub
          // bounds:                 |-----|
          else empty
        //                   lower   upper
        // range:               |-----|
        //             lb     ub
        // bounds:      |-----|
        else empty
      else empty
    }

    /**
     * Returns range of elements between `lower` (including) and `upper` (including).
     * 
     * Preconditions:
     * <tr>1. `lower ≤ upper`</tr>
     * <tr>2. `order.lowerBound ≤ lower ≤ order.upperBound`</tr>
     * <tr>3. `order.lowerBound ≤ upper ≤ order.upperBound`</tr>
     */
    protected def cons(lower: E, upper: E): R & Range.NonEmpty[E]
  }
}