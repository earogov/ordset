package ordset.core

import ordset.core.domain._
import ordset.{Order, Show, util}
import ordset.util.label.Label
import ordset.util.types.SingleValue

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

/**
 * Bound of [[Segment]] or [[Interval]]. Bounds are represented by 
 * <tr>- some value of type `E`;</tr>
 * <tr>- value inclusion indicator;</tr>
 * <tr>- bound type: upper or lower.</tr>
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
 */
sealed trait Bound[@sp(spNum) +E] {

  /**
   * Returns bound value.
   */
  def value: E

  /**
   * Returns `true` if bound is upper.
   */
  def isUpper: Boolean

  /**
   * Returns `true` if bound is lower.
   */
  def isLower: Boolean = !isUpper

  /**
   * Returns `true` if bound includes `value`.
   */
  def isInclusive: Boolean

  /**
   * Returns `true` if bound doesn't include `value`.
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
}

object Bound {

  implicit def lowerToInterval[E, D <: Domain[E]](lower: Lower[E])(
    implicit domainOps: DomainOps[E, D]): Interval[E, D] = domainOps.interval(lower)

  implicit def upperToInterval[E, D <: Domain[E]](upper: Upper[E])(
    implicit domainOps: DomainOps[E, D]): Interval[E, D] = domainOps.interval(upper)

  implicit def defaultAscOrder[E](implicit elementOrd: AscOrder[E], intOrder: AscOrder[Int]): AscOrder[Bound[E]] =
    new DefaultOrder(elementOrd, intOrder)

  def defaultDescOrder[E](implicit elementOrd: DescOrder[E], intOrder: DescOrder[Int]): DescOrder[Bound[E]] =
    new DefaultOrder(elementOrd, intOrder)

  implicit def defaultShow[E](implicit elementShow: Show[E]): Show[Bound[E]] =
    SetBuilderFormat.boundShow(elementShow)

  case class Upper[@sp(spNum) +E](override val value: E, override val isInclusive: Boolean) extends Bound[E] {

    override def isUpper: Boolean = true

    override def isLower: Boolean = false

    override def provideUpper: Bound.Upper[E] = this

    override def provideLower: Bound.Lower[E] = this.flipUpper
    
    override def flip: Bound[E] = this.flipUpper

    def flipUpper: Lower[E] = Lower(value, !isInclusive)

    override def offset: Int = if (isInclusive) 0 else -1

    override def toString: String = SetBuilderFormat.upperBound(this, (e: E) => e.toString)
  }

  object Upper {

    def inclusive[@sp(spNum) E](element: E): Bound.Upper[E] = Upper(element, isInclusive = true)

    def exclusive[@sp(spNum) E](element: E): Bound.Upper[E] = Upper(element, isInclusive = false)

    def max[E](x: Upper[E], y: Upper[E])(implicit order: Order[Bound[E]]): Upper[E] =
      order.max(x, y).asInstanceOf[Upper[E]]

    def min[E](x: Upper[E], y: Upper[E])(implicit order: Order[Bound[E]]): Upper[E] =
      order.min(x, y).asInstanceOf[Upper[E]]
  }

  case class Lower[@sp(spNum) +E](override val value: E, override val isInclusive: Boolean) extends Bound[E] {

    override def isUpper: Boolean = false

    override def isLower: Boolean = true

    override def provideUpper: Bound.Upper[E] = this.flipLower

    override def provideLower: Bound.Lower[E] = this
    
    override def flip: Bound[E] = this.flipLower

    def flipLower: Upper[E] = Upper(value, !isInclusive)

    override def offset: Int = if (isInclusive) 0 else 1

    override def toString: String = SetBuilderFormat.lowerBound(this, (e: E) => e.toString)
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
      val cmp = elementOrd.compare(x.value, y.value)
      if (cmp != 0) cmp
      else intOrd.compare(x.offset, y.offset)
    }

    override def hash(x: Bound[E]): Int =
      product2Hash(elementOrd.hash(x.value), intOrd.hash(x.offset))

    override def eqv(x: Bound[E], y: Bound[E]): Boolean =
      elementOrd.eqv(x.value, y.value) && intOrd.eqv(x.offset, y.offset)
  }
}
