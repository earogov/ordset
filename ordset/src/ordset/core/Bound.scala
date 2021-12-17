package ordset.core

import ordset.core.domain.*
import ordset.core.interval.*
import ordset.{Order, Hash, Discrete, Show, util}

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

/**
 * Bound defines subset of ordered set of elements `E`.
 * <tr>[[Upper]] bound defines maximal element in subset (including or excluding);</tr>
 * <tr>[[Lower]] bound defines minimal element in subset (including or excluding).</tr>
 * <tr></tr>
 *
 * Bound is described by:
 * <tr>- some element of ordered set;</tr>
 * <tr>- inclusion flag;</tr>
 * <tr>- bound type (upper or lower).</tr>
 *
 * {{{
 *  lower including    upper including
 *       bound             bound
 *        v                 v
 *        [0--------------10](-------------)15
 *                           ^             ^
 *                  lower excluding   upper excluding
 *                      bound              bound
 * }}}
 *
 * @tparam E type of element
 *
 * @see [[ExtendedBound]]
 */
sealed trait Bound[@sp(spNum) +E] extends ExtendedBound[E] {

  /**
   * Returns bound element.
   */
  def element: E

  /**
   * Returns `true` if bound includes `element`.
   */
  def isIncluding: Boolean

  /**
   * Returns `true` if bound doesn't include `element`.
   */
  def isExcluding: Boolean = !isIncluding

  /**
   * Returns [[Bound.flip]] if bound is lower or bound itself otherwise.
   */
  def provideUpper: Bound.Upper[E]

  /**
   * Returns [[Bound.flip]] if bound is upper or bound itself otherwise.
   */
  def provideLower: Bound.Lower[E]

  /**
   * Let b1 is initial bound then b2 = b1.flip is such bound that:
   * <tr>b2.value = b1.value</tr>
   * <tr>b2.isIncluding = !b1.isIncluding</tr>
   * <tr>b2.isUpper = !b1.isUpper</tr>
   * {{{
   *            bound                 bound
   *             v                      v
   *   --------](--------       --------](--------
   *          ^                          ^
   *      bound.flip                 bound.flip
   * }}}
   */
  def flip: Bound[E]

  /**
   * Returns
   * <tr> 0 - if bound is including;</tr>
   * <tr> 1 - if bound is lower and excluding;</tr>
   * <tr>-1 - if bound is upper and excluding.</tr>
   * <tr></tr>
   *
   * Offset defines bounds ordering when their values are equal.
   */
  def offset: Int

  /**
   * @return new bound with specified element. New bound has the same type (upper or lower) and inclusion flag
   *         as original one.
   */
  def withElement[F](element: F): Bound[F]

  /**
   * @return new bound with value mapped by `mapFunc`. New bound has the same type (upper or lower) and inclusion flag
   *         as original one.
   */
  def mapElement[F](mapFunc: E => F): Bound[F] = withElement(mapFunc(element))

  override def isLimited: Boolean = true

  override def isUnlimited: Boolean = false

  override def isBelowAll: Boolean = false

  override def isAboveAll: Boolean = false

  override def flipLimited: ExtendedBound[E]
  
  override def extendedOffset: Int = 0
}

object Bound {

  implicit def lowerToInterval[E, D <: Domain[E]](lower: Lower[E])(implicit ops: DomainOps[E, D]): Interval[E, D] =
    ops.intervals.builder.aboveBound(lower)

  implicit def upperToInterval[E, D <: Domain[E]](upper: Upper[E])(implicit ops: DomainOps[E, D]): Interval[E, D] =
    ops.intervals.builder.belowBound(upper)

  implicit def continuousUnboundedOrder[E](
    implicit elementOrd: Order[E] with Hash[E]
  ): ContinuousUnboundedOrder[E] =
    new ContinuousUnboundedOrderImpl(elementOrd)

  implicit def continuousBoundedBelowOrder[E](
    implicit elementOrd: ordset.BoundedOrder.Below[E, E] with Hash[E]
  ): ContinuousBoundedBelowOrder[E] =
    new ContinuousBoundedBelowOrderImpl(elementOrd)

  implicit def continuousBoundedAboveOrder[E](
    implicit elementOrd: ordset.BoundedOrder.Above[E, E] with Hash[E]
  ): ContinuousBoundedAboveOrder[E] =
    new ContinuousBoundedAboveOrderImpl(elementOrd)

  implicit def continuousBoundedOrder[E](
    implicit elementOrd: ordset.BoundedOrder[E, E, E] with Hash[E]
  ): ContinuousBoundedOrder[E] =
    new ContinuousBoundedOrderImpl(elementOrd)

  implicit def discreteUnboundedOrder[E](
    implicit elementOrd: ordset.DiscreteOrder[E] with Discrete.Infinite[E] with Hash[E]
  ): DiscreteUnboundedOrder[E] =
    new DiscreteUnboundedOrderImpl(elementOrd)

  implicit def discreteBoundedBelowOrder[E](
    implicit elementOrd: ordset.DiscreteOrder[E] with ordset.BoundedOrder.Below[E, E] with Hash[E]
  ): DiscreteBoundedBelowOrder[E] =
    new DiscreteBoundedBelowOrderImpl(elementOrd)

  implicit def discreteBoundedAboveOrder[E](
    implicit elementOrd: ordset.DiscreteOrder[E] with ordset.BoundedOrder.Above[E, E] with Hash[E]
  ): DiscreteBoundedAboveOrder[E] =
    new DiscreteBoundedAboveOrderImpl(elementOrd)

  implicit def discreteBoundedOrder[E](
    implicit elementOrd: ordset.DiscreteOrder[E] with ordset.BoundedOrder[E, E, E] with Hash[E]
  ): DiscreteBoundedOrder[E] =
    new DiscreteBoundedOrderImpl(elementOrd)

  implicit def defaultShow[E](implicit elementShow: Show[E]): Show[Bound[E]] =
    SetBuilderFormat.boundShow(elementShow)

  /**
   * Bound that defines maximal element in subset (including or excluding) of ordered set.
   *
   * @tparam E type of element
   */
  final case class Upper[@sp(spNum) +E](
    override val element: E,
    override val isIncluding: Boolean
  ) extends Bound[E]
    with ExtendedBound.Upper[E] {

    override def provideUpper: Bound.Upper[E] = this

    override def provideLower: Bound.Lower[E] = this.flipUpper

    override def flip: Bound[E] = this.flipUpper

    override def flipLimited: ExtendedBound[E] = this.flipUpper
    
    def flipUpper: Lower[E] = Lower(element, !isIncluding)

    override def offset: Int = if (isIncluding) 0 else -1

    override def withElement[F](element: F): Bound[F] = Upper(element, isIncluding)

    override def toString: String = SetBuilderFormat.upperBound(this, SetBuilderFormat.toStringFunc[E])
  }

  object Upper {

    def including[@sp(spNum) E](element: E): Bound.Upper[E] = Upper(element, isIncluding = true)

    def excluding[@sp(spNum) E](element: E): Bound.Upper[E] = Upper(element, isIncluding = false)

    def max[E](x: Upper[E], y: Upper[E])(implicit order: Order[Bound[E]]): Upper[E] =
      order.max(x, y).asInstanceOf[Upper[E]]

    def min[E](x: Upper[E], y: Upper[E])(implicit order: Order[Bound[E]]): Upper[E] =
      order.min(x, y).asInstanceOf[Upper[E]]
  }

  /**
   * Bound that defines minimal element in subset (including or excluding) of ordered set.
   *
   * @tparam E type of element
   */
  final case class Lower[@sp(spNum) +E](
    override val element: E,
    override val isIncluding: Boolean
  ) extends Bound[E]
    with ExtendedBound.Lower[E] {

    override def isUpper: Boolean = false

    override def isLower: Boolean = true

    override def provideUpper: Bound.Upper[E] = this.flipLower

    override def provideLower: Bound.Lower[E] = this

    override def flip: Bound[E] = this.flipLower

    override def flipLimited: ExtendedBound[E] = this.flipLower

    def flipLower: Upper[E] = Upper(element, !isIncluding)

    override def offset: Int = if (isIncluding) 0 else 1

    override def withElement[F](element: F): Bound[F] = Lower(element, isIncluding)

    override def toString: String = SetBuilderFormat.lowerBound(this, SetBuilderFormat.toStringFunc[E])
  }

  object Lower {

    def including[@sp(spNum) E](element: E): Bound.Lower[E] = Lower(element, isIncluding = true)

    def excluding[@sp(spNum) E](element: E): Bound.Lower[E] = Lower(element, isIncluding = false)

    def max[E](x: Lower[E], y: Lower[E])(implicit order: Order[Bound[E]]): Lower[E] =
      order.max(x, y).asInstanceOf[Lower[E]]

    def min[E](x: Lower[E], y: Lower[E])(implicit order: Order[Bound[E]]): Lower[E] =
      order.min(x, y).asInstanceOf[Lower[E]]
  }

  sealed trait DefaultOrder[E] extends Order[Bound[E]] with Hash[Bound[E]] {

    def elementOrd: Order[E] with Hash[E]
  }

  sealed trait BoundedBelowOrder[E] 
    extends ordset.BoundedOrder.Below.Including[Bound[E], Bound.Lower[E]]
    with DefaultOrder[E] {

    override def elementOrd: ordset.BoundedOrder.Below[E, E] with Hash[E]

    final override val lowerBound: Bound.Lower[E] = 
      new Bound.Lower(elementOrd.lowerBound, elementOrd.lowerBoundIncluded)
  }

  sealed trait BoundedAboveOrder[E] 
    extends ordset.BoundedOrder.Above.Including[Bound[E], Bound.Upper[E]]
    with DefaultOrder[E] {

    override def elementOrd: ordset.BoundedOrder.Above[E, E] with Hash[E]

    final override val upperBound: Bound.Upper[E] = 
      new Bound.Upper(elementOrd.upperBound, elementOrd.upperBoundIncluded)
  }

  sealed trait BoundedOrder[E]
    extends ordset.BoundedOrder.Including[Bound[E], Bound.Lower[E], Bound.Upper[E]]
    with BoundedBelowOrder[E]
    with BoundedAboveOrder[E] {

    override def elementOrd: ordset.BoundedOrder[E, E, E] with Hash[E]
  }

  sealed trait ContinuousOrder[E] extends DefaultOrder[E] {

    import util.HashUtil._
    import ordset.givens.int.{intNaturalOrder => intOrd}

    final override def compare(x: Bound[E], y: Bound[E]): Int = {
      val cmp = elementOrd.compare(x.element, y.element)
      if (cmp != 0) cmp
      else intOrd.compare(x.offset, y.offset)
    }

    final override def hash(x: Bound[E]): Int =
      product2Hash(elementOrd.hash(x.element), intOrd.hash(x.offset))

    final override def eqv(x: Bound[E], y: Bound[E]): Boolean =
      elementOrd.eqv(x.element, y.element) && intOrd.eqv(x.offset, y.offset)
  }

  trait ContinuousUnboundedOrder[E] extends ContinuousOrder[E]

  trait ContinuousBoundedBelowOrder[E] extends BoundedBelowOrder[E] with ContinuousOrder[E]

  trait ContinuousBoundedAboveOrder[E] extends BoundedAboveOrder[E] with ContinuousOrder[E]

  trait ContinuousBoundedOrder[E] 
    extends BoundedOrder[E]
    with ContinuousBoundedBelowOrder[E]
    with ContinuousBoundedAboveOrder[E] {

    override def elementOrd: ordset.BoundedOrder[E, E, E] with Hash[E]
  }

  sealed trait DiscreteOrder[E] extends DefaultOrder[E] {

    import util.HashUtil._
    import ordset.givens.int.{intNaturalOrder => intOrd}

    override def elementOrd: ordset.DiscreteOrder[E] with Hash[E]

    override def compare(x: Bound[E], y: Bound[E]): Int = {
      var xe = x.element
      var xo = x.offset
      if (x.isExcluding) {
        // ( N  =>  [ N+1
        if (x.isLower) {
          val xs = elementOrd.successorOrNull(xe)
          if (xs != null) {
            xe = xs
            xo = 0
          }
        // N )  =>  N-1 ]
        } else {
          val xp = elementOrd.predecessorOrNull(xe)
          if (xp != null) {
            xe = xp
            xo = 0
          }
        }
      }
      var ye = y.element
      var yo = y.offset
      if (y.isExcluding) {
        // ( N  =>  [ N+1
        if (y.isLower) {
          val ys = elementOrd.successorOrNull(ye)
          if (ys != null) {
            ye = ys
            yo = 0
          }
        // N )  =>  N-1 ]
        } else {
          val yp = elementOrd.predecessorOrNull(ye)
          if (yp != null) {
            ye = yp
            yo = 0
          }
        }
      }
      val cmp = elementOrd.compare(xe, ye)
      if (cmp != 0) cmp
      else intOrd.compare(xo, yo)
    }

    override def hash(x: Bound[E]): Int = {
      var xe = x.element
      var xo = x.offset
      if (x.isExcluding) {
        // ( N  =>  [ N+1
        if (x.isLower) {
          val xs = elementOrd.successorOrNull(xe)
          if (xs != null) {
            xe = xs
            xo = 0
          }
        // N )  =>  N-1 ]
        } else {
          val xp = elementOrd.predecessorOrNull(xe)
          if (xp != null) {
            xe = xp
            xo = 0
          }
        }
      }
      product2Hash(elementOrd.hash(xe), intOrd.hash(xo))
    }

    override def eqv(x: Bound[E], y: Bound[E]): Boolean = {
      var xe = x.element
      var xo = x.offset
      if (x.isExcluding) {
        // ( N  =>  [ N+1
        if (x.isLower) {
          val xs = elementOrd.successorOrNull(xe)
          if (xs != null) {
            xe = xs
            xo = 0
          }
        // N )  =>  N-1 ]
        } else {
          val xp = elementOrd.predecessorOrNull(xe)
          if (xp != null) {
            xe = xp
            xo = 0
          }
        }
      }
      var ye = y.element
      var yo = y.offset
      if (y.isExcluding) {
        // ( N  =>  [ N+1
        if (y.isLower) {
          val ys = elementOrd.successorOrNull(ye)
          if (ys != null) {
            ye = ys
            yo = 0
          }
        // N )  =>  N-1 ]
        } else {
          val yp = elementOrd.predecessorOrNull(ye)
          if (yp != null) {
            ye = yp
            yo = 0
          }
        }
      }
      elementOrd.eqv(xe, ye) && intOrd.eqv(xo, yo)
    }
  }

  trait DiscreteUnboundedOrder[E] extends DiscreteOrder[E] {

    import util.HashUtil._
    import ordset.givens.int.{intNaturalOrder => intOrd}

    override def elementOrd: ordset.DiscreteOrder[E] with Discrete.Infinite[E] with Hash[E]

    final override def compare(x: Bound[E], y: Bound[E]): Int = {
      var xe = x.element
      var xo = x.offset
      if (x.isExcluding) {
        // ( N  =>  [ N+1
        if (x.isLower) {
          xe = elementOrd.successor(xe)
          xo = 0
        // N )  =>  N-1 ]
        } else {
          xe = elementOrd.predecessor(xe)
          xo = 0
        }
      }
      var ye = y.element
      var yo = y.offset
      if (y.isExcluding) {
        // ( N  =>  [ N+1
        if (y.isLower) {
          ye = elementOrd.successor(ye)
          yo = 0
        // N )  =>  N-1 ]
        } else {
          ye = elementOrd.predecessor(ye)
          yo = 0
        }
      }
      val cmp = elementOrd.compare(xe, ye)
      if (cmp != 0) cmp
      else intOrd.compare(xo, yo)
    }

    final override def hash(x: Bound[E]): Int = {
      var xe = x.element
      var xo = x.offset
      if (x.isExcluding) {
        // ( N  =>  [ N+1
        if (x.isLower) {
          xe = elementOrd.successor(xe)
          xo = 0
        // N )  =>  N-1 ]
        } else {
          xe = elementOrd.predecessor(xe)
          xo = 0
        }
      }
      product2Hash(elementOrd.hash(xe), intOrd.hash(xo))
    }

    final override def eqv(x: Bound[E], y: Bound[E]): Boolean = {
      var xe = x.element
      var xo = x.offset
      if (x.isExcluding) {
        // ( N  =>  [ N+1
        if (x.isLower) {
          xe = elementOrd.successor(xe)
          xo = 0
        // N )  =>  N-1 ]
        } else {
          xe = elementOrd.predecessor(xe)
          xo = 0
        }
      }
      var ye = y.element
      var yo = y.offset
      if (y.isExcluding) {
        // ( N  =>  [ N+1
        if (y.isLower) {
          ye = elementOrd.successor(ye)
          yo = 0
        // N )  =>  N-1 ]
        } else {
          ye = elementOrd.predecessor(ye)
          yo = 0
        }
      }
      elementOrd.eqv(xe, ye) && intOrd.eqv(xo, yo)
    }
  }

  trait DiscreteBoundedBelowOrder[E] extends BoundedBelowOrder[E] with DiscreteOrder[E] {
  
    override def elementOrd: ordset.DiscreteOrder[E] with ordset.BoundedOrder.Below[E, E] with Hash[E]

    final override def compare(x: Bound[E], y: Bound[E]): Int = super.compare(x, y)

    final override def hash(x: Bound[E]): Int = super.hash(x)

    final override def eqv(x: Bound[E], y: Bound[E]): Boolean = super.eqv(x, y)
  }

  trait DiscreteBoundedAboveOrder[E] extends BoundedAboveOrder[E] with DiscreteOrder[E] {

    override def elementOrd: ordset.DiscreteOrder[E] with ordset.BoundedOrder.Above[E, E] with Hash[E]

    final override def compare(x: Bound[E], y: Bound[E]): Int = super.compare(x, y)

    final override def hash(x: Bound[E]): Int = super.hash(x)

    final override def eqv(x: Bound[E], y: Bound[E]): Boolean = super.eqv(x, y)
  }

  trait DiscreteBoundedOrder[E] extends BoundedOrder[E] with DiscreteOrder[E] {

    override def elementOrd: ordset.DiscreteOrder[E] with ordset.BoundedOrder[E, E, E] with Hash[E]

    final override def compare(x: Bound[E], y: Bound[E]): Int = super.compare(x, y)

    final override def hash(x: Bound[E]): Int = super.hash(x)

    final override def eqv(x: Bound[E], y: Bound[E]): Boolean = super.eqv(x, y)
  }

  // Private section ---------------------------------------------------------- //
  private final class ContinuousUnboundedOrderImpl[E](
    override val elementOrd: Order[E] with Hash[E]
  ) extends ContinuousUnboundedOrder[E]

  private final class ContinuousBoundedBelowOrderImpl[E](
    override val elementOrd: ordset.BoundedOrder.Below[E, E] with Hash[E]
  ) extends ContinuousBoundedBelowOrder[E]

  private final class ContinuousBoundedAboveOrderImpl[E](
    override val elementOrd: ordset.BoundedOrder.Above[E, E] with Hash[E]
  ) extends ContinuousBoundedAboveOrder[E]

  private final class ContinuousBoundedOrderImpl[E](
    override val elementOrd: ordset.BoundedOrder[E, E, E] with Hash[E]
  ) extends ContinuousBoundedOrder[E]

  private final class DiscreteUnboundedOrderImpl[E](
    override val elementOrd: ordset.DiscreteOrder[E] with Discrete.Infinite[E] with Hash[E]
  ) extends DiscreteUnboundedOrder[E]

  private final class DiscreteBoundedBelowOrderImpl[E](
    override val elementOrd: ordset.DiscreteOrder[E] with ordset.BoundedOrder.Below[E, E] with Hash[E]
  ) extends DiscreteBoundedBelowOrder[E]

  private final class DiscreteBoundedAboveOrderImpl[E](
    override val elementOrd: ordset.DiscreteOrder[E] with ordset.BoundedOrder.Above[E, E] with Hash[E]
  ) extends DiscreteBoundedAboveOrder[E]

  private final class DiscreteBoundedOrderImpl[E](
    override val elementOrd: ordset.DiscreteOrder[E] with ordset.BoundedOrder[E, E, E] with Hash[E]
  ) extends DiscreteBoundedOrder[E]
}

/**
 * Extension of [[Bound]] with unlimited cases:
 * <tr>[[ExtendedBound.BelowAll]] - unlimited bound that is less then any other;</tr>
 * <tr>[[ExtendedBound.AboveAll]] - unlimited bound that is greater then any other;</tr>
 * <tr>[[Bound]] - standard (limited) bound.</tr>
 * <tr></tr>
 *
 * Unlimited cases don't have any value associated with them.
 *
 * {{{
 *  below all                 above all
 *      v                        v
 *      X---------5](------------X
 *                 ^
 *            limited bound
 * }}}
 *
 * @tparam E type of element
 */
sealed trait ExtendedBound[@sp(spNum) +E] {

  /**
   * @return `true` if bound is limited, i.e. is subtype of [[Bound]].
   */
  def isLimited: Boolean

  /**
   * @return `true` if bound is unlimited, i.e. is one of [[ExtendedBound.BelowAll]] or [[ExtendedBound.AboveAll]].
   */
  def isUnlimited: Boolean

  /**
   * @return `true` if bound defines maximal element in subset (including or excluding) in case of limited bound or
   *         the absence of restrictions on such element in case of unlimited bound.
   */
  def isUpper: Boolean

  /**
   * @return `true` if bound defines minimal element in subset (including or excluding) in case of limited bound or
   *         the absence of restrictions on such element in case of unlimited bound.
   */
  def isLower: Boolean

  /**
   * @return `true` if bound is less then any other, i.e. if it's [[ExtendedBound.BelowAll]].
   */
  def isBelowAll: Boolean

  /**
   * @return `true` if bound is greater then any other, i.e. if it's [[ExtendedBound.AboveAll]].
   */
  def isAboveAll: Boolean

  /**
   * Returns
   * <tr>flipped bound (see [[Bound.flip]]) - if bound is limited returns;</tr>
   * <tr>bound itself - if bound is unlimited.</tr>
   */
  def flipLimited: ExtendedBound[E]
  
  /**
   * Returns
   * <tr> 0 - if bound is limited;</tr>
   * <tr> 1 - if bound is [[ExtendedBound.AboveAll]];</tr>
   * <tr>-1 - if bound is [[ExtendedBound.BelowAll]].</tr>
   * <tr></tr>
   *
   * Offset defines bounds ordering.
   */
  def extendedOffset: Int
}

object ExtendedBound {

  implicit def unboundedOrder[E](implicit boundOrd: Bound.DefaultOrder[E]): UnboundedOrder[E] =
    new UnboundedOrderImpl(boundOrd)

  implicit def boundedBelowOrder[E](implicit boundOrd: Bound.BoundedBelowOrder[E]): BoundedBelowOrder[E] =
    new BoundedBelowOrderImpl(boundOrd)

  implicit def boundedAboveOrder[E](implicit boundOrd: Bound.BoundedAboveOrder[E]): BoundedAboveOrder[E] =
    new BoundedAboveOrderImpl(boundOrd)

  implicit def boundedOrder[E](implicit boundOrd: Bound.BoundedOrder[E]): BoundedOrder[E] =
    new BoundedOrderImpl(boundOrd)

  implicit def defaultShow[E](implicit elementShow: Show[E]): Show[ExtendedBound[E]] =
    SetBuilderFormat.extendedBoundShow(elementShow)

  /**
   * Bound that defines maximal element in subset (including or excluding) of ordered set.
   * If bound is unlimited then there is no restrictions on maximal element in subset.
   *
   * @tparam E type of element
   */
  sealed trait Upper[@sp(spNum) +E] extends ExtendedBound[E] {

    override def isUpper: Boolean = true

    override def isLower: Boolean = false
  }

  /**
   * Bound that defines minimal element in subset (including or excluding) of ordered set.
   * If bound is unlimited then there is no restrictions on minimal element in subset.
   *
   * @tparam E type of element
   */
  sealed trait Lower[@sp(spNum) +E] extends ExtendedBound[E] {

    override def isUpper: Boolean = false

    override def isLower: Boolean = true
  }

  /**
   * Unlimited bound that is greater then any other bound.
   */
  case object AboveAll extends ExtendedBound.Upper[Nothing] {

    override val isLimited: Boolean = false

    override val isUnlimited: Boolean = true

    override val isBelowAll: Boolean = false

    override val isAboveAll: Boolean = true

    override val isUpper: Boolean = true

    override val isLower: Boolean = false

    override def flipLimited: ExtendedBound[Nothing] = this

    override val extendedOffset: Int = 1

    override def toString: String = SetBuilderFormat.aboveAllBound
  }

  /**
   * Unlimited bound that is less then any other bound.
   */
  case object BelowAll extends ExtendedBound.Lower[Nothing] {

    override val isLimited: Boolean = false

    override val isUnlimited: Boolean = true

    override val isBelowAll: Boolean = true

    override val isAboveAll: Boolean = false

    override val isUpper: Boolean = false

    override val isLower: Boolean = true

    override def flipLimited: ExtendedBound[Nothing] = this

    override val extendedOffset: Int = -1

    override def toString: String = SetBuilderFormat.belowAllBound
  }

  sealed trait DefaultOrder[E] 
    extends ordset.BoundedOrder.Including[ExtendedBound[E], ExtendedBound[E], ExtendedBound[E]] 
    with Hash[ExtendedBound[E]] {

    import ordset.givens.int.{intNaturalOrder => intOrd}

    def boundOrd: Order[Bound[E]] with Hash[Bound[E]]

    final override def compare(x: ExtendedBound[E], y: ExtendedBound[E]): Int = {
      val cmp = intOrd.compare(x.extendedOffset, y.extendedOffset)
      if (cmp != 0) cmp
      else (x, y) match {
        case (x: Bound[E], y: Bound[E]) => boundOrd.compare(x, y)
        case _ => 0
      }
    }

    final override def hash(x: ExtendedBound[E]): Int =
      x match {
        case x: Bound[E] => boundOrd.hash(x)
        case _ => x.hashCode()
      }

    final override def eqv(x: ExtendedBound[E], y: ExtendedBound[E]): Boolean = {
      val eqv = intOrd.eqv(x.extendedOffset, y.extendedOffset)
      if (!eqv) false
      else (x, y) match {
        case (x: Bound[E], y: Bound[E]) => boundOrd.eqv(x, y)
        case _ => true
      }
    }
  }

  trait UnboundedOrder[E] 
    extends ordset.BoundedOrder.Including[ExtendedBound[E], ExtendedBound.BelowAll.type, ExtendedBound.AboveAll.type]
    with DefaultOrder[E] {

    override def boundOrd: Bound.DefaultOrder[E]

    final override val lowerBound: ExtendedBound.BelowAll.type = ExtendedBound.BelowAll

    final override val upperBound: ExtendedBound.AboveAll.type = ExtendedBound.AboveAll
  }

  trait BoundedBelowOrder[E] 
    extends ordset.BoundedOrder.Including[ExtendedBound[E], Bound.Lower[E], ExtendedBound.AboveAll.type]
    with DefaultOrder[E] {

    override def boundOrd: Bound.BoundedBelowOrder[E]

    final override val lowerBound: Bound.Lower[E] = boundOrd.lowerBound

    final override val upperBound: ExtendedBound.AboveAll.type = ExtendedBound.AboveAll
  }

  trait BoundedAboveOrder[E] 
    extends ordset.BoundedOrder.Including[ExtendedBound[E], ExtendedBound.BelowAll.type, Bound.Upper[E]]
    with DefaultOrder[E] {

    override def boundOrd: Bound.BoundedAboveOrder[E]

    final override val lowerBound: ExtendedBound.BelowAll.type = ExtendedBound.BelowAll

    final override val upperBound: Bound.Upper[E] = boundOrd.upperBound
  }

  trait BoundedOrder[E] 
    extends ordset.BoundedOrder.Including[ExtendedBound[E], Bound.Lower[E], Bound.Upper[E]]
    with DefaultOrder[E] {

    override def boundOrd: Bound.BoundedOrder[E]

    final override val lowerBound: Bound.Lower[E] = boundOrd.lowerBound

    final override val upperBound: Bound.Upper[E] = boundOrd.upperBound
  }

  // Private section ---------------------------------------------------------- //
  private final class UnboundedOrderImpl[E](
    override val boundOrd: Bound.DefaultOrder[E]
  ) extends UnboundedOrder[E]

  private final class BoundedBelowOrderImpl[E](
    override val boundOrd: Bound.BoundedBelowOrder[E]
  ) extends BoundedBelowOrder[E]

  private final class BoundedAboveOrderImpl[E](
    override val boundOrd: Bound.BoundedAboveOrder[E]
  ) extends BoundedAboveOrder[E]

  private final class BoundedOrderImpl[E](
    override val boundOrd: Bound.BoundedOrder[E]
  ) extends BoundedOrder[E]
}