package ordset

import ordset.domain.{AscOrder, DescOrder, DirectedOrder, Domain, DomainOps, OrderDir}
import ordset.util.label.Label
import ordset.util.types.SingleValue

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

sealed trait Bound[@sp(spNum) +E] {

  def value: E

  def isUpper: Boolean

  def isLower: Boolean = !isUpper

  def isInclusive: Boolean

  def isExclusive: Boolean = !isInclusive

  def flip: Bound[E]

  def offset: Int
}

object Bound {

  implicit def lowerToInterval[E, D <: Domain[E]](lower: Lower[E])(
    implicit domainOps: DomainOps[E, D]): Interval[E, D] = domainOps.interval(lower)

  implicit def upperToInterval[E, D <: Domain[E]](upper: Upper[E])(
    implicit domainOps: DomainOps[E, D]): Interval[E, D] = domainOps.interval(upper)

  implicit def defaultAscOrder[E](implicit elementOrd: AscOrder[E]): AscOrder[Bound[E]] =
    new DefaultOrder(elementOrd, instances.Int.intAscOrder)

  def defaultDescOrder[E](implicit elementOrd: DescOrder[E]): DescOrder[Bound[E]] =
    new DefaultOrder(elementOrd, instances.Int.intDescOrder)

  case class Upper[+E](override val value: E, override val isInclusive: Boolean) extends Bound[E] {

    override def isUpper: Boolean = true

    override def isLower: Boolean = false

    override def flip: Bound[E] = Lower(value, !isInclusive)

    def flipUpper: Lower[E] = Lower(value, !isInclusive)

    override def offset: Int = if (isInclusive) 0 else -1

    override def toString: String = s"x ${if (isInclusive) "<=" else "<"} $value"
  }

  object Upper {

    def inclusive[E](element: E): Bound.Upper[E] = Upper(element, isInclusive = true)

    def exclusive[E](element: E): Bound.Upper[E] = Upper(element, isInclusive = false)

    def max[E](x: Upper[E], y: Upper[E])(implicit order: Order[Bound[E]]): Upper[E] =
      order.max(x, y).asInstanceOf[Upper[E]]

    def min[E](x: Upper[E], y: Upper[E])(implicit order: Order[Bound[E]]): Upper[E] =
      order.min(x, y).asInstanceOf[Upper[E]]
  }

  case class Lower[+E](override val value: E, override val isInclusive: Boolean) extends Bound[E] {

    override def isUpper: Boolean = false

    override def isLower: Boolean = true

    override def flip: Bound[E] = Upper(value, !isInclusive)

    def flipLower: Upper[E] = Upper(value, !isInclusive)

    override def offset: Int = if (isInclusive) 0 else 1

    override def toString: String = s"x ${if (isInclusive) ">=" else ">"} $value"
  }

  object Lower {

    def inclusive[E](element: E): Bound.Lower[E] = Lower(element, isInclusive = true)

    def exclusive[E](element: E): Bound.Lower[E] = Lower(element, isInclusive = false)

    def max[E](x: Lower[E], y: Lower[E])(implicit order: Order[Bound[E]]): Lower[E] =
      order.max(x, y).asInstanceOf[Lower[E]]

    def min[E](x: Lower[E], y: Lower[E])(implicit order: Order[Bound[E]]): Lower[E] =
      order.min(x, y).asInstanceOf[Lower[E]]
  }

  class DefaultOrder[E, Dir <: OrderDir](
      val elementOrd: DirectedOrder[E, Dir],
      val intOrd: DirectedOrder[Int, Dir]
  )(
      implicit val dirValue: SingleValue[Dir]
  ) extends DirectedOrder.Abstract[Bound[E], Dir] {

    import util.Hash._

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
