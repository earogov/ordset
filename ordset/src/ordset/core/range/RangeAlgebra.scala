package ordset.core.range

import ordset.Order

/**
 * Typeclass with range operators.
 * 
 * @tparam E type of range elements.
 * @tparam R type of range.
 */
trait RangeAlgebra[E] {

  /**
   * Returns `true`, if:
   * <tr>- range 'r' is empty;</tr>
   * <tr>- range `r` is non-empty and `r.lower ≤ r.upper` according to given `order`.</tr>
   */
  def isValid(r: Range[E])(implicit order: Order[E]): Boolean =
    r match {
      case r: Range.NonEmpty[E] => isValidNE(r)
      case _ => true
    }

  /**
   * Returns `true`, if `r.lower ≤ r.upper` according to given `order`.
   */
  def isValidNE(r: Range.NonEmpty[E])(implicit order: Order[E]): Boolean =
    order.lteqv(r.lower, r.upper)

  /**
   * Returns `true` if range `r` contains element `e`.
   * 
   * Preconditions:
   * <tr>1. Range `r` must be valid according to given `order` (see [[isValid]]).</tr>
   */
  def contains(r: Range[E], e: E)(implicit order: Order[E]): Boolean = 
    r match {
      case r: Range.NonEmpty[E] => containsNE(r, e)
      case _ => false
    }

  /**
   * Returns `true` if non-empty range `r` contains element `e`.
   * 
   * Preconditions:
   * <tr>1. Range `r` must be valid according to given `order` (see [[isValid]]).</tr>
   */
  def containsNE(r: Range.NonEmpty[E], e: E)(implicit order: Order[E]): Boolean = 
    order.lteqv(r.lower, e) && order.gteqv(r.upper, e)

  /**
   * Returns element `e`, if it is inside range `r`, otherwise returns bound of `r` closest to element.
   * {{{
   * 
   *      e                        - input
   *      | 
   *         |-------------|       - r
   *         |         
   *      r.lower                  - output
   *
   *                e              - input
   *                | 
   *         |-------------|       - r
   *                |         
   *                e              - output
   * 
   *                          e    - input
   *                          | 
   *         |-------------|       - r
   *                       |         
   *                    r.upper    - output
   * }}}
   * 
   * Preconditions:
   * <tr>1. Range `r` must be valid according to given `order` (see [[isValid]]).</tr>
   */
  def restrictNE(r: Range.NonEmpty[E], e: E)(implicit order: Order[E]): E =
    if order.lteqv(e, r.lower) then r.lower
    else if order.lteqv(r.upper, e) then r.upper
    else e

  /**
   * Returns range of elements that belongs to range `r` and not less than element `e`.
   * {{{
   *
   *                e
   *                |
   *         |-------------|  - r
   *
   *                |------|  - output
   *                e
   * }}}
   * 
   * Preconditions:
   * <tr>1. Range `r` must be valid according to given `order` (see [[isValid]]).</tr>
   */
  def takeAbove[R <: Range[E]](r: Range[E], e: E)(implicit range: RangeFactory[E, R]): R =
    r match {
      case r: Range.NonEmpty[E] => takeAboveNE(r, e)
      case _ => range.empty
    }

  /**
   * Returns range of elements that belongs to non-empty range `r` and not less than element `e`.
   * {{{
   *
   *                e
   *                |
   *         |-------------|  - r
   *
   *                |------|  - output
   *                e
   * }}}
   * 
   * Preconditions:
   * <tr>1. Range `r` must be valid according to given `order` (see [[isValid]]).</tr>
   */
  def takeAboveNE[R <: Range[E]](r: Range.NonEmpty[E], e: E)(implicit range: RangeFactory[E, R]): R =
    //       e
    //       |-----------------
    // r:         |-----|
    if range.order.lteqv(e, r.lower) then range.likeNE(r)
    //               e
    //               |---------
    // r:         |-----|
    //
    //                     e
    //                     |---
    // r:         |-----|
    else range.between(e, r.upper)

  /**
   * Returns range of elements that belongs to range `r` and not greater than element `e`.
   * {{{
   *
   *                e
   *                |
   *         |-------------|  - r
   *
   *         |------|         - output
   *                e
   * }}}
   * 
   * Preconditions:
   * <tr>1. Range `r` must be valid according to given `order` (see [[isValid]]).</tr>
   */
  def takeBelow[R <: Range[E]](r: Range[E], e: E)(implicit range: RangeFactory[E, R]): R =
    r match {
      case r: Range.NonEmpty[E] => takeBelowNE(r, e)
      case _ => range.empty
    }

  /**
   * Returns range of elements that belongs to non-empty range `r` and not greater than element `e`.
   * {{{
   *
   *                e
   *                |
   *         |-------------|  - r
   *
   *         |------|         - output
   *                e
   * }}}
   * 
   * Preconditions:
   * <tr>1. Range `r` must be valid according to given `order` (see [[isValid]]).</tr>
   */
  def takeBelowNE[R <: Range[E]](r: Range.NonEmpty[E], e: E)(implicit range: RangeFactory[E, R]): R =
    //                        e
    //       -----------------|
    // r:         |-----|
    if range.order.gteqv(e, r.upper) then range.likeNE(r)
    //               e
    //       --------|
    // r:         |-----|
    //
    //          e
    //       ---|
    // r:         |-----|
    else range.between(r.lower, e)

  /**
   * Returns range of elements that belongs to both ranges `x` and `y`.
   * {{{
   * 
   *         |-------------|  - x
   *
   *    |-----------|         - y
   * 
   *         |------|         - output
   * }}}
   * 
   * Preconditions:
   * <tr>1. Ranges `x` and `y` must be valid according to given `order` (see [[isValid]]).</tr>
   */
  def cross[R <: Range[E]](x: Range[E], y: Range[E])(implicit range: RangeFactory[E, R]): R = 
    (x, y) match {
      case (x: Range.NonEmpty[E], y: Range.NonEmpty[E]) => crossNE(x, y)
      case _ => range.empty
    }

  /**
   * Returns range of elements that belongs to both non-empty ranges `x` and `y`.
   * {{{
   * 
   *         |-------------|  - x
   *
   *    |-----------|         - y
   * 
   *         |------|         - output
   * }}}
   * 
   * Preconditions:
   * <tr>1. Ranges `x` and `y` must be valid according to given `order` (see [[isValid]]).</tr>
   */
  def crossNE[R <: Range[E]](x: Range.NonEmpty[E], y: Range.NonEmpty[E])(implicit range: RangeFactory[E, R]): R = 
    // x:         |-----|
    // y:            |-|
    //
    // x:         |-----|
    // y:            |-----|
    //
    // x:         |-----|
    // y:                 |-----|
    if range.order.lteqv(x.lower, y.lower) then takeBelowNE(y, x.upper)
    // x:            |-|
    // y:         |-----|
    //
    // x:            |-----|
    // y:         |-----|
    //
    // x:                 |-----|
    // y:         |-----|
    else takeBelowNE(x, y.upper)

  /**
   * Returns range spanning ranges `x` and `y`.
   * 
   * If one of the ranges is empty and another is non-empty, returns non-empty one.
   * If both ranges are empty, returns empty range.
   * {{{
   * 
   *      |------|            - x
   * 
   *                |------|  - y
   * 
   *      |----------------|  - output
   * 
   * }}}
   * 
   * Preconditions:
   * <tr>1. Ranges `x` and `y` must be valid according to given `order` (see [[isValid]]).</tr>
   */
  def span[R <: Range[E]](x: Range[E], y: Range[E])(implicit range: RangeFactory[E, R]): R =
    (x, y) match {
      case (x: Range.NonEmpty[E], y: Range.NonEmpty[E]) => spanNE(x, y)
      case (x: Range.NonEmpty[E], _) => range.likeNE(x)
      case (_, y: Range.NonEmpty[E]) => range.likeNE(y)
      case _ => range.empty
    }

  /**
   * Returns range spanning non-empty ranges `x` and `y`.
   * {{{
   * 
   *      |------|            - x
   * 
   *                |------|  - y
   * 
   *      |----------------|  - output
   * 
   * }}}
   * 
   * Preconditions:
   * <tr>1. Ranges `x` and `y` must be valid according to given `order` (see [[isValid]]).</tr>
   */
  def spanNE[R <: Range[E]](x: Range.NonEmpty[E], y: Range.NonEmpty[E])(implicit range: RangeFactory[E, R]): R = {
    val ord = range.order
    range.between(ord.min(x.lower, y.lower), ord.max(x.upper, y.upper))
  }
}

object RangeAlgebra {

  /**
   * Returns default implementation of range algebra.
   */
  implicit def defaultAlgebra[E]: RangeAlgebra[E] = defaultAlgebraInstance.asInstanceOf

  // Private section ---------------------------------------------------------- //
  private val defaultAlgebraInstance: RangeAlgebra[Any] = new RangeAlgebra {}
}