package ordset.core

import ordset.core.Bound.DefaultOrder
import ordset.core.domain.*
import ordset.core.interval.*
import ordset.{Order, Show, util}
import ordset.util.label.Label
import ordset.util.types.SingleValue

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

/**
 * Bound defines subset of ordered set of elements `E`.
 * <tr>[[Upper]] bound defines maximal element in subset (inclusive or exclusive);</tr>
 * <tr>[[Lower]] bound defines minimal element in subset (inclusive or exclusive).</tr>
 * <tr></tr>
 *
 * Bound is described by:
 * <tr>- some element of ordered set;</tr>
 * <tr>- inclusion flag;</tr>
 * <tr>- bound type (upper or lower).</tr>
 *
 * {{{
 *  lower inclusive    upper inclusive
 *       bound             bound
 *        v                 v
 *        [0--------------10](-------------)15
 *                           ^             ^
 *                  lower exclusive   upper exclusive
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
  def isInclusive: Boolean

  /**
   * Returns `true` if bound doesn't include `element`.
   */
  def isExclusive: Boolean = !isInclusive

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
   * <tr>b2.isInclusive = !b1.isInclusive</tr>
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
   * <tr> 0 - if bound is inclusive;</tr>
   * <tr> 1 - if bound is lower and exclusive;</tr>
   * <tr>-1 - if bound is upper and exclusive.</tr>
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
    ops.interval(lower)

  implicit def upperToInterval[E, D <: Domain[E]](upper: Upper[E])(implicit ops: DomainOps[E, D]): Interval[E, D] =
    ops.interval(upper)

  implicit def defaultAscOrder[E](implicit elementOrd: AscOrder[E], intOrder: AscOrder[Int]): AscOrder[Bound[E]] =
    new DefaultOrder(elementOrd, intOrder)

  def defaultDescOrder[E](implicit elementOrd: DescOrder[E], intOrder: DescOrder[Int]): DescOrder[Bound[E]] =
    new DefaultOrder(elementOrd, intOrder)

  implicit def defaultShow[E](implicit elementShow: Show[E]): Show[Bound[E]] =
    SetBuilderFormat.boundShow(elementShow)

  /**
   * Bound that defines maximal element in subset (inclusive or exclusive) of ordered set.
   *
   * @tparam E type of element
   */
  final case class Upper[@sp(spNum) +E](
    override val element: E,
    override val isInclusive: Boolean
  ) extends Bound[E]
    with ExtendedBound.Upper[E] {

    override def provideUpper: Bound.Upper[E] = this

    override def provideLower: Bound.Lower[E] = this.flipUpper

    override def flip: Bound[E] = this.flipUpper

    override def flipLimited: ExtendedBound[E] = this.flipUpper
    
    def flipUpper: Lower[E] = Lower(element, !isInclusive)

    override def offset: Int = if (isInclusive) 0 else -1

    override def withElement[F](element: F): Bound[F] = Upper(element, isInclusive)

    override def toString: String = SetBuilderFormat.upperBound(this, SetBuilderFormat.toStringFunc[E])
  }

  object Upper {

    def inclusive[@sp(spNum) E](element: E): Bound.Upper[E] = Upper(element, isInclusive = true)

    def exclusive[@sp(spNum) E](element: E): Bound.Upper[E] = Upper(element, isInclusive = false)

    def max[E](x: Upper[E], y: Upper[E])(implicit order: Order[Bound[E]]): Upper[E] =
      order.max(x, y).asInstanceOf[Upper[E]]

    def min[E](x: Upper[E], y: Upper[E])(implicit order: Order[Bound[E]]): Upper[E] =
      order.min(x, y).asInstanceOf[Upper[E]]
  }

  /**
   * Bound that defines minimal element in subset (inclusive or exclusive) of ordered set.
   *
   * @tparam E type of element
   */
  final case class Lower[@sp(spNum) +E](
    override val element: E,
    override val isInclusive: Boolean
  ) extends Bound[E]
    with ExtendedBound.Lower[E] {

    override def isUpper: Boolean = false

    override def isLower: Boolean = true

    override def provideUpper: Bound.Upper[E] = this.flipLower

    override def provideLower: Bound.Lower[E] = this

    override def flip: Bound[E] = this.flipLower

    override def flipLimited: ExtendedBound[E] = this.flipLower

    def flipLower: Upper[E] = Upper(element, !isInclusive)

    override def offset: Int = if (isInclusive) 0 else 1

    override def withElement[F](element: F): Bound[F] = Lower(element, isInclusive)

    override def toString: String = SetBuilderFormat.lowerBound(this, SetBuilderFormat.toStringFunc[E])
  }

  object Lower {

    def inclusive[@sp(spNum) E](element: E): Bound.Lower[E] = Lower(element, isInclusive = true)

    def exclusive[@sp(spNum) E](element: E): Bound.Lower[E] = Lower(element, isInclusive = false)

    def max[E](x: Lower[E], y: Lower[E])(implicit order: Order[Bound[E]]): Lower[E] =
      order.max(x, y).asInstanceOf[Lower[E]]

    def min[E](x: Lower[E], y: Lower[E])(implicit order: Order[Bound[E]]): Lower[E] =
      order.min(x, y).asInstanceOf[Lower[E]]
  }

  final class DefaultOrder[E, Dir <: OrderDir](
      val elementOrd: DirectedOrder[E, Dir],
      val intOrd: DirectedOrder[Int, Dir]
  )(
      implicit val dirValue: SingleValue[Dir]
  ) extends DirectedOrder.Abstract[Bound[E], Dir] {

    import util.HashUtil._

    override val label: Label = OrderLabels.BoundDefault

    override def compare(x: Bound[E], y: Bound[E]): Int = {
      val cmp = elementOrd.compare(x.element, y.element)
      if (cmp != 0) cmp
      else intOrd.compare(x.offset, y.offset)
    }

    override def hash(x: Bound[E]): Int =
      product2Hash(elementOrd.hash(x.element), intOrd.hash(x.offset))

    override def eqv(x: Bound[E], y: Bound[E]): Boolean =
      elementOrd.eqv(x.element, y.element) && intOrd.eqv(x.offset, y.offset)
  }
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
   * @return `true` if bound defines maximal element in subset (inclusive or exclusive) in case of limited bound or
   *         the absence of restrictions on such element in case of unlimited bound.
   */
  def isUpper: Boolean

  /**
   * @return `true` if bound defines minimal element in subset (inclusive or exclusive) in case of limited bound or
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

  implicit def defaultAscOrder[E](
    implicit boundOrd: AscOrder[Bound[E]],
    intOrder: AscOrder[Int]
  ): AscOrder[ExtendedBound[E]] =
    new DefaultOrder(boundOrd, intOrder)

  def defaultDescOrder[E](
    implicit boundOrd: DescOrder[Bound[E]],
    intOrder: DescOrder[Int]
  ): DescOrder[ExtendedBound[E]] =
    new DefaultOrder(boundOrd, intOrder)

  implicit def defaultShow[E](implicit elementShow: Show[E]): Show[ExtendedBound[E]] =
    SetBuilderFormat.extendedBoundShow(elementShow)

  /**
   * Bound that defines maximal element in subset (inclusive or exclusive) of ordered set.
   * If bound is unlimited then there is no restrictions on maximal element in subset.
   *
   * @tparam E type of element
   */
  sealed trait Upper[@sp(spNum) +E] extends ExtendedBound[E] {

    override def isUpper: Boolean = true

    override def isLower: Boolean = false
  }

  /**
   * Bound that defines minimal element in subset (inclusive or exclusive) of ordered set.
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

  final class DefaultOrder[E, Dir <: OrderDir](
    val boundOrd: DirectedOrder[Bound[E], Dir],
    val intOrd: DirectedOrder[Int, Dir]
  )(
    implicit val dirValue: SingleValue[Dir]
  ) extends DirectedOrder.Abstract[ExtendedBound[E], Dir] {

    override val label: Label = OrderLabels.BoundDefault

    override def compare(x: ExtendedBound[E], y: ExtendedBound[E]): Int = {
      val cmp = intOrd.compare(x.extendedOffset, y.extendedOffset)
      if (cmp != 0) cmp
      else (x, y) match {
        case (x: Bound[E], y: Bound[E]) => boundOrd.compare(x, y)
        case _ => 0
      }
    }

    override def hash(x: ExtendedBound[E]): Int =
      x match {
        case x: Bound[E] => boundOrd.hash(x)
        case _ => x.hashCode()
      }

    override def eqv(x: ExtendedBound[E], y: ExtendedBound[E]): Boolean = {
      val eqv = intOrd.eqv(x.extendedOffset, y.extendedOffset)
      if (!eqv) false
      else (x, y) match {
        case (x: Bound[E], y: Bound[E]) => boundOrd.eqv(x, y)
        case _ => true
      }
    }
  }
}