package ordset.core.range

import ordset.Order

trait RangeAlgebra[E] {

  def between[R <: Range[E]](
    lower: E, 
    upper: E
  )(
    implicit 
    range: RangeFactory[E, R], 
    ord: Order[E]
  ): R = ???

  def contains(r: Range[E], e: E)(implicit ord: Order[E]): Boolean = 
    r match {
      case r: Range.NonEmpty[E] => containsNE(r, e)
      case _ => false
    }

  def containsNE(r: Range.NonEmpty[E], e: E)(implicit ord: Order[E]): Boolean = 
    ord.lteqv(r.lower, e) && ord.gteqv(r.upper, e)

  def takeAbove[R <: Range[E]](
    r: Range[E], 
    e: E
  )(
    implicit 
    range: RangeFactory[E, R], 
    ord: Order[E]
  ): R =
    r match {
      case r: Range.NonEmpty[E] => takeAboveNE(r, e)
      case _ => range.empty
    }

  def takeAboveNE[R <: Range[E]](
    r: Range.NonEmpty[E], 
    e: E
  )(
    implicit 
    range: RangeFactory[E, R], 
    ord: Order[E]
  ): R =
    //       e
    //       |-----------------
    // r:         |-----|
    if ord.lteqv(e, r.lower) then range.like(r)
    //               e
    //               |---------
    // r:         |-----|
    else if ord.lteqv(e, r.upper) then range.between(e, r.upper)
    //                     e
    //                     |---
    // r:         |-----|
    else range.empty

  def takeBelow[R <: Range[E]](
    r: Range[E], 
    e: E
  )(
    implicit 
    range: RangeFactory[E, R], 
    ord: Order[E]
  ): R =
    r match {
      case r: Range.NonEmpty[E] => takeBelowNE(r, e)
      case _ => range.empty
    }

  def takeBelowNE[R <: Range[E]](
    r: Range.NonEmpty[E], 
    e: E
  )(
    implicit 
    range: RangeFactory[E, R], 
    ord: Order[E]
  ): R =
    //                        e
    //       -----------------|
    // r:         |-----|
    if ord.gteqv(e, r.upper) then range.like(r)
    //               e
    //       --------|
    // r:         |-----|
    else if ord.gteqv(e, r.lower) then range.between(r.lower, e)
    //          e
    //       ---|
    // r:         |-----|
    else range.empty

  def cross[R <: Range[E]](
    x: Range[E], 
    y: Range[E]
  )(
    implicit 
    range: RangeFactory[E, R], 
    ord: Order[E]
  ): R = 
    (x, y) match {
      case (x: Range.NonEmpty[E], y: Range.NonEmpty[E]) => crossNE(x, y)
      case _ => range.empty
    }

  def crossNE[R <: Range[E]](
    x: Range.NonEmpty[E], 
    y: Range.NonEmpty[E]
  )(
    implicit 
    range: RangeFactory[E, R], 
    ord: Order[E]
  ): R = 
    if ord.lteqv(x.lower, y.upper) then
      if ord.lteqv(y.lower, x.lower) then
        // x:         |-----|
        // y:      |-----|
        if ord.lt(y.upper, x.upper) then range.between(x.lower, y.upper)
        // x:         |-----|
        // y:      |-----------|
        else range.like(x)
      else if ord.lteqv(y.lower, x.upper) then
        // x:         |-----|
        // y:            |-----|
        if ord.lt(x.upper, y.upper) then range.between(y.lower, x.upper)
        // x:         |-----|
        // y:           |-|
        else range.like(y)
      // x:         |-----|
      // y:                 |-----|
      else range.empty
    // x:              |-----|
    // y:      |-----|
    else range.empty
}

object RangeAlgebra {

  implicit def defaultAlg[E](): RangeAlgebra[E] = algebraInstance.asInstanceOf

  // Private section ---------------------------------------------------------- //
  private val algebraInstance: RangeAlgebra[Any] = new RangeAlgebra {}
}