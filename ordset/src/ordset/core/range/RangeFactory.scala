package ordset.core.range

import ordset.{ContravariantOrder, ContravariantBoundedOrder}

/**
 * Typeclass with capability to construct ranges.
 * 
 * @tparam E type of range elements.
 * @tparam R type of range.
 */
trait RangeFactory[-E, +R[_] <: Range[_]] {

  /**
   * Order typeclass for elements.
   */
  def order: ContravariantOrder[E]

  /** 
   * Returns empty range.
   */
  def empty[EE <: E]: R[EE] & Range.Empty

  /** 
   * Returns range of elements between `lower` (including) and `upper` (including).
   * 
   * Returns [[Range.Empty]], if there is no element `e` in domain that satisfies condition:
   * 
   * `lower ≤ e ≤ upper`
   */
  def between[EE <: E](lower: EE, upper: EE): R[EE]

  /** 
   * Returns range of elements between lower bound of range `r` (including) and upper bound of `r` (including).
   * 
   * Returns [[Range.Empty]], if there is no element `e` in domain that satisfies condition:
   * 
   * `r.lower ≤ e ≤ r.upper`
   */
  def like[EE <: E](r: Range[EE]): R[EE] = 
    r match {
      case r: Range.NonEmpty[EE] => likeNE(r)
      case _ => empty
    }

  /** 
   * Returns range of elements between lower bound of range `r` (including) and upper bound of `r` (including).
   * 
   * Returns [[Range.Empty]], if there is no element `e` in domain that satisfies condition:
   * 
   * `r.lower ≤ e ≤ r.upper`
   */
  def likeNE[EE <: E](r: Range.NonEmpty[EE]): R[EE] = between(r.lower, r.upper)
}

object RangeFactory {

  /**
   * Typeclass with capability to construct ranges on unbounded domain.
   * 
   * @tparam E type of range elements.
   * @tparam R type of range.
   */
  trait Unbounded[-E, +R[_] <: Range[_]] extends RangeFactory[E, R] {

    override def between[EE <: E](lower: EE, upper: EE): R[EE] =
      if order.lteqv(lower, upper) then cons(lower, upper)
      else empty

    /**
     * Returns range of elements between `lower` (including) and `upper` (including).
     * 
     * Preconditions:
     * <tr>1. `lower ≤ upper`</tr>
     */
    protected def cons[EE <: E](lower: EE, upper: EE): R[EE] & Range.NonEmpty[EE]
  }

  /**
   * Typeclass with capability to construct ranges on bounded from below domain.
   * 
   * @tparam E type of range elements.
   * @tparam L type of lower bound of domain.
   * @tparam R type of range.
   */
  trait BoundedBelow[-E, +L <: E, +R[_] <: Range[_]] extends RangeFactory[E, R] {

    override def order: ContravariantBoundedOrder.Below[E, L]

    override def between[EE >: L <: E](lower: EE, upper: EE): R[EE] = {
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
    protected def cons[EE <: E](lower: EE, upper: EE): R[EE] & Range.NonEmpty[EE]
  }

  /**
   * Typeclass with capability to construct ranges on bounded from above domain.
   * 
   * @tparam E type of range elements.
   * @tparam U type of upper bound of domain.
   * @tparam R type of range.
   */
  trait BoundedAbove[-E, +U <: E, +R[_] <: Range[_]] extends RangeFactory[E, R] {

    override def order: ContravariantBoundedOrder.Above[E, U]

    override def between[EE >: U <: E](lower: EE, upper: EE): R[EE] = {
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
    protected def cons[EE <: E](lower: EE, upper: EE): R[EE] & Range.NonEmpty[EE]
  }

  /**
   * Typeclass with capability to construct ranges on bounded domain.
   * 
   * @tparam E type of range elements.
   * @tparam L type of lower bound of domain.
   * @tparam U type of upper bound of domain.
   * @tparam R type of range.
   */
  trait Bounded[-E, +L <: E, +U <: E, +R[_] <: Range[_]] extends RangeFactory[E, R] {

    override def order: ContravariantBoundedOrder[E, L, U]

    override def between[EE >: L | U <: E](lower: EE, upper: EE): R[EE] = {
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
    protected def cons[EE <: E](lower: EE, upper: EE): R[EE] & Range.NonEmpty[EE]
  }
}