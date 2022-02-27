package ordset.core.range

import ordset.{ContravariantOrder, ContravariantBoundedOrder}

/**
 * Typeclass with capability to construct ranges.
 * 
 * @tparam E1 lower type bound of range elements.
 * @tparam E2 upper type bound of range elements.
 * @tparam R type of range.
 */
trait RangeFactory[+E1 <: E2, -E2, +R[+X] <: Range[_]] {

  /**
   * Order typeclass for elements.
   */
  def order: ContravariantOrder[E2]

  /** 
   * Returns empty range.
   */
  def empty: R[Nothing] & Range.Empty

  /** 
   * Returns range of elements between `lower` (including) and `upper` (including).
   * 
   * Returns [[Range.Empty]], if there is no element `e` in domain that satisfies condition:
   * 
   * `lower ≤ e ≤ upper`
   * 
   * @tparam E type of range elements.
   */
  def between[E >: E1 <: E2](lower: E, upper: E): R[E]

  /** 
   * Returns range of elements between lower bound of range `r` (including) and upper bound of `r` (including).
   * 
   * Returns [[Range.Empty]], if there is no element `e` in domain that satisfies condition:
   * 
   * `r.lower ≤ e ≤ r.upper`
   * 
   * @tparam E type of range elements.
   */
  def like[E >: E1 <: E2](r: Range[E]): R[E] = 
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
   * 
   * @tparam E type of range elements.
   */
  def likeNE[E >: E1 <: E2](r: Range.NonEmpty[E]): R[E] = between(r.lower, r.upper)
}

object RangeFactory {

  /**
   * Typeclass with capability to construct ranges on unbounded domain.
   * 
   * @tparam E1 lower type bound of range elements.
   * @tparam E2 upper type bound of range elements.
   * @tparam R type of range.
   */
  trait Unbounded[+E1 <: E2, -E2, +R[+X] <: Range[_]] extends RangeFactory[E1, E2, R] {

    override def between[E >: E1 <: E2](lower: E, upper: E): R[E] =
      if order.lteqv(lower, upper) then cons(lower, upper)
      else empty

    /**
     * Returns range of elements between `lower` (including) and `upper` (including).
     * 
     * Preconditions:
     * <div>1. `lower ≤ upper`</div>
     * 
     * @tparam E type of range elements.
     */
    protected def cons[E >: E1 <: E2](lower: E, upper: E): R[E] & Range.NonEmpty[E]
  }

  /**
   * Typeclass with capability to construct ranges on bounded from below domain.
   * 
   * @tparam E1 lower type bound of range elements.
   * @tparam E2 upper type bound of range elements.
   * @tparam R type of range.
   */
  trait BoundedBelow[+E1 <: E2, -E2, +R[+X] <: Range[_]] extends RangeFactory[E1, E2, R] {

    override def order: ContravariantBoundedOrder.Below[E2, E1]

    override def between[E >: E1 <: E2](lower: E, upper: E): R[E] = {
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
     * <div>1. `lower ≤ upper`</div>
     * <div>2. `order.lowerBound ≤ lower`</div>
     * <div>3. `order.lowerBound ≤ upper`</div>
     * 
     * @tparam E type of range elements.
     */
    protected def cons[E >: E1 <: E2](lower: E, upper: E): R[E] & Range.NonEmpty[E]
  }

  /**
   * Typeclass with capability to construct ranges on bounded from above domain.
   * 
   * @tparam E1 lower type bound of range elements.
   * @tparam E2 upper type bound of range elements.
   * @tparam R type of range.
   */
  trait BoundedAbove[+E1 <: E2, -E2, +R[+X] <: Range[_]] extends RangeFactory[E1, E2, R] {

    override def order: ContravariantBoundedOrder.Above[E2, E1]

    override def between[E >: E1 <: E2](lower: E, upper: E): R[E] = {
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
     * <div>1. `lower ≤ upper`</div>
     * <div>2. `lower ≤ order.upperBound`</div>
     * <div>3. `upper ≤ order.upperBound`</div>
     * 
     * @tparam E type of range elements.
     */
    protected def cons[E >: E1 <: E2](lower: E, upper: E): R[E] & Range.NonEmpty[E]
  }

  /**
   * Typeclass with capability to construct ranges on bounded domain.
   * 
   * @tparam E1 lower type bound of range elements.
   * @tparam E2 upper type bound of range elements.
   * @tparam R type of range.
   */
  trait Bounded[+E1 <: E2, -E2, +R[+X] <: Range[_]] extends RangeFactory[E1, E2, R] {

    override def order: ContravariantBoundedOrder[E2, E1, E1]

    override def between[E >: E1 <: E2](lower: E, upper: E): R[E] = {
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
     * <div>1. `lower ≤ upper`</div>
     * <div>2. `order.lowerBound ≤ lower ≤ order.upperBound`</div>
     * <div>3. `order.lowerBound ≤ upper ≤ order.upperBound`</div>
     * 
     * @tparam E type of range elements.
     */
    protected def cons[E >: E1 <: E2](lower: E, upper: E): R[E] & Range.NonEmpty[E]
  }
}