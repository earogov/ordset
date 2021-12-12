package ordset.core

import ordset.core.Bound.DefaultOrder
import ordset.core.domain.*
import ordset.core.interval.*
import ordset.{BoundedOrder, Order, Hash, Show, util}

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

  implicit def defaultUnboundedOrder[E](
    implicit elementOrd: Order[E] with Hash[E]
  ): DefaultUnboundedOrder[E] =
    new DefaultUnboundedOrderImpl(elementOrd)

  def defaultBoundedBelowOrder[E, L <: Bound[E], U <: Bound[E]](
    implicit elementOrd: BoundedOrder.Below[E, E] with Hash[E]
  ): DefaultBoundedBelowOrder[E] =
    new DefaultBoundedBelowOrderImpl(elementOrd)

  def defaultBoundedAboveOrder[E, L <: Bound[E], U <: Bound[E]](
    implicit elementOrd: BoundedOrder.Above[E, E] with Hash[E]
  ): DefaultBoundedAboveOrder[E] =
    new DefaultBoundedAboveOrderImpl(elementOrd)

  def defaultBoundedOrder[E, L <: Bound[E], U <: Bound[E]](
    implicit elementOrd: BoundedOrder[E, E, E] with Hash[E]
  ): DefaultBoundedOrder[E] =
    new DefaultBoundedOrderImpl(elementOrd)

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

  trait DefaultOrder[E] extends Order[Bound[E]] with Hash[Bound[E]] {

    import util.HashUtil._
    import ordset.instances.int.{intOrderWithHash => intOrd}

    def elementOrd: Order[E] with Hash[E]

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

  trait DefaultUnboundedOrder[E] extends DefaultOrder[E]

  trait DefaultBoundedBelowOrder[E] 
    extends BoundedOrder.Below.Including[Bound[E], Bound.Lower[E]]
    with DefaultOrder[E] {

    override def elementOrd: BoundedOrder.Below[E, E] with Hash[E]

    final override val lowerBound: Bound.Lower[E] = 
      new Bound.Lower(elementOrd.lowerBound, elementOrd.lowerBoundIncluded)
  }

  trait DefaultBoundedAboveOrder[E] 
    extends BoundedOrder.Above.Including[Bound[E], Bound.Upper[E]]
    with DefaultOrder[E] {

    override def elementOrd: BoundedOrder.Above[E, E] with Hash[E]

    final override val upperBound: Bound.Upper[E] = 
      new Bound.Upper(elementOrd.upperBound, elementOrd.upperBoundIncluded) 
  }

  trait DefaultBoundedOrder[E] 
    extends BoundedOrder.Including[Bound[E], Bound.Lower[E], Bound.Upper[E]]
    with DefaultBoundedBelowOrder[E]
    with DefaultBoundedAboveOrder[E] {

    override def elementOrd: BoundedOrder[E, E, E] with Hash[E]
  }

  // Private section ---------------------------------------------------------- //
  private final class DefaultUnboundedOrderImpl[E](
    override val elementOrd: Order[E] with Hash[E]
  ) extends DefaultUnboundedOrder[E]

  private final class DefaultBoundedBelowOrderImpl[E](
    override val elementOrd: BoundedOrder.Below[E, E] with Hash[E]
  ) extends DefaultBoundedBelowOrder[E]

  private final class DefaultBoundedAboveOrderImpl[E](
    override val elementOrd: BoundedOrder.Above[E, E] with Hash[E]
  ) extends DefaultBoundedAboveOrder[E]

  private final class DefaultBoundedOrderImpl[E](
    override val elementOrd: BoundedOrder[E, E, E] with Hash[E]
  ) extends DefaultBoundedOrder[E]
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

  implicit def defaultUnboundedOrder[E](
    implicit boundOrd: Bound.DefaultUnboundedOrder[E]
  ): DefaultUnboundedOrder[E] =
    new DefaultUnboundedOrderImpl(boundOrd)

  def defaultBoundedBelowOrder[E](
    implicit boundOrd: Bound.DefaultBoundedBelowOrder[E]
  ): DefaultBoundedBelowOrder[E] =
    new DefaultBoundedBelowOrderImpl(boundOrd)

  def defaultBoundedAboveOrder[E](
    implicit boundOrd: Bound.DefaultBoundedAboveOrder[E]
  ): DefaultBoundedAboveOrder[E] =
    new DefaultBoundedAboveOrderImpl(boundOrd)

  def defaultBoundedOrder[E](
    implicit boundOrd: Bound.DefaultBoundedOrder[E]
  ): DefaultBoundedOrder[E] =
    new DefaultBoundedOrderImpl(boundOrd)

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

  trait DefaultOrder[E]
    extends BoundedOrder.Including[ExtendedBound[E], ExtendedBound[E], ExtendedBound[E]] 
    with Hash[ExtendedBound[E]] {

    import ordset.instances.int.{intOrderWithHash => intOrd}

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

  trait DefaultUnboundedOrder[E] 
    extends BoundedOrder.Including[ExtendedBound[E], ExtendedBound.BelowAll.type, ExtendedBound.AboveAll.type]
    with DefaultOrder[E] {

    override def boundOrd: Bound.DefaultUnboundedOrder[E]

    final override val lowerBound: ExtendedBound.BelowAll.type = ExtendedBound.BelowAll

    final override val upperBound: ExtendedBound.AboveAll.type = ExtendedBound.AboveAll
  }

  trait DefaultBoundedBelowOrder[E] 
    extends BoundedOrder.Including[ExtendedBound[E], Bound.Lower[E], ExtendedBound.AboveAll.type]
    with DefaultOrder[E] {

    override def boundOrd: Bound.DefaultBoundedBelowOrder[E]

    final override val lowerBound: Bound.Lower[E] = boundOrd.lowerBound

    override val upperBound: ExtendedBound.AboveAll.type = ExtendedBound.AboveAll
  }

  trait DefaultBoundedAboveOrder[E] 
    extends BoundedOrder.Including[ExtendedBound[E], ExtendedBound.BelowAll.type, Bound.Upper[E]]
    with DefaultOrder[E] {

    override def boundOrd: Bound.DefaultBoundedAboveOrder[E]

    override val lowerBound: ExtendedBound.BelowAll.type = ExtendedBound.BelowAll

    final override val upperBound: Bound.Upper[E] = boundOrd.upperBound
  }

  trait DefaultBoundedOrder[E] 
    extends BoundedOrder.Including[ExtendedBound[E], Bound.Lower[E], Bound.Upper[E]]
    with DefaultOrder[E] {

    override def boundOrd: Bound.DefaultBoundedOrder[E]

    final override val lowerBound: Bound.Lower[E] = boundOrd.lowerBound

    final override val upperBound: Bound.Upper[E] = boundOrd.upperBound
  }

  // Private section ---------------------------------------------------------- //
  private final class DefaultUnboundedOrderImpl[E](
    val boundOrd: Bound.DefaultUnboundedOrder[E]
  ) extends DefaultUnboundedOrder[E]

  private final class DefaultBoundedBelowOrderImpl[E](
    val boundOrd: Bound.DefaultBoundedBelowOrder[E]
  ) extends DefaultBoundedBelowOrder[E]

  private final class DefaultBoundedAboveOrderImpl[E](
    val boundOrd: Bound.DefaultBoundedAboveOrder[E]
  ) extends DefaultBoundedAboveOrder[E]

  private final class DefaultBoundedOrderImpl[E](
    val boundOrd: Bound.DefaultBoundedOrder[E]
  ) extends DefaultBoundedOrder[E]
}