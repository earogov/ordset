package ordset

import ordset.util.SingleValue

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

  def apply[E](element: E): Bound[E] = Upper(element, isInclusive = true)

  implicit def toRightUnbounded[E](bound: Lower[E]): Interval.RightUnbounded[E] = Interval.RightUnbounded(bound)

  implicit def toLeftUnbounded[E](bound: Upper[E]): Interval.LeftUnbounded[E] = Interval.LeftUnbounded(bound)

  implicit def defaultAscOrder[E](implicit elemOrd: AscOrder[E]): AscOrder[Bound[E]] = new DefaultOrder(elemOrd)

  implicit def defaultDescOrder[E](implicit elemOrd: DescOrder[E]): DescOrder[Bound[E]] = new DefaultOrder(elemOrd)

  implicit def defaultHash[E](implicit hash: Hash[E]): Hash[Bound[E]] = new DefaultHash[E]()(hash)


  case class Upper[+E](override val value: E, override val isInclusive: Boolean) extends Bound[E] {

    override def isUpper: Boolean = true

    override def isLower: Boolean = false

    override def flip: Bound[E] = Lower(value, !isInclusive)

    def flipUpper: Lower[E] = Lower(value, !isInclusive)

    override def offset: Int = if (isInclusive) 0 else -1

    override def toString: String = s"x ${if (isInclusive) "<=" else "<"} $value"
  }

  case class Lower[+E](override val value: E, override val isInclusive: Boolean) extends  Bound[E] {

    override def isUpper: Boolean = false

    override def isLower: Boolean = true

    override def flip: Bound[E] = Upper(value, !isInclusive)

    def flipLower: Upper[E] = Upper(value, !isInclusive)

    override def offset: Int = if (isInclusive) 0 else 1

    override def toString: String = s"x ${if (isInclusive) ">=" else ">"} $value"
  }

  class DefaultOrder[E, Dir <: OrderDir](
      val elementOrd: OrderWithDir[E, Dir]
  )(
      implicit
      val intOrd: OrderWithDir[Int, Dir],
      override protected val dirValue: SingleValue[Dir]
  ) extends OrderWithDir.Abstract[Bound[E], Dir] {

    override def compare(x: Bound[E], y: Bound[E]): Int = {
      val cmp = elementOrd.compare(x.value, y.value)
      if (cmp != 0) cmp
      else intOrd.compare(x.offset, y.offset)
    }
  }

  class DefaultHash[E](implicit hashE: Hash[E]) extends Hash[Bound[E]] {
    import util.Hash._

    override def hash(x: Bound[E]): Int = product2Hash(hashE.hash(x.value), x.offset.##)
    override def eqv(x: Bound[E], y: Bound[E]): Boolean = hashE.eqv(x.value, y.value) && x.offset == y.offset
  }
}
