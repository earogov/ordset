package ordset

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

sealed trait Interval[@sp(spNum) +E] {

  def ->[@sp(Boolean) V](value: V): IntervalMapping[E, V] = IntervalMapping(this, value)
}

object Interval {

  def unbounded(): Unbounded.type = Unbounded

  def rightUnbounded[@sp(spNum) E](value: E, isInclusive: Boolean): RightUnbounded[E] =
    RightUnbounded(Bound.Lower(value, isInclusive))

  def leftUnbounded[@sp(spNum) E](value: E, isInclusive: Boolean): LeftUnbounded[E] =
    LeftUnbounded(Bound.Upper(value, isInclusive))

  def bounded[@sp(spNum) E](leftVal: E, leftIncl: Boolean, rightVal: E, rightIncl: Boolean): Bounded[E] =
    Bounded(Bound.Lower(leftVal, leftIncl), Bound.Upper(rightVal, rightIncl))

  implicit def defaultHash[E](implicit hash: Hash[Bound[E]]): Hash[Interval[E]] = new DefaultHash[E]()(hash)

  case object Unbounded extends Interval[Nothing] {

    override def toString: String = "x in U"
  }

  case class RightUnbounded[+E](leftBound: Bound.Lower[E]) extends Interval[E] {

    override def toString: String = leftBound.toString
  }

  case class LeftUnbounded[+E](rightBound: Bound.Upper[E]) extends Interval[E] {

    override def toString: String = rightBound.toString
  }

  case class Bounded[+E](leftBound: Bound.Lower[E], rightBound: Bound.Upper[E]) extends Interval[E] {

    override def toString: String = s"$leftBound & $rightBound"
  }

  class DefaultHash[E](implicit hashB: Hash[Bound[E]]) extends Hash[Interval[E]] {
    import util.Hash._

    override def hash(x: Interval[E]): Int = x match {
      case Bounded(l, r) => product2Hash(hashB.hash(l), hashB.hash(r))
      case RightUnbounded(l) => product1Hash(hashB.hash(l))
      case LeftUnbounded(r) => product1Hash(hashB.hash(r))
      case u: Unbounded.type => u.hashCode()
    }

    override def eqv(x: Interval[E], y: Interval[E]): Boolean = (x, y) match {
      case (Unbounded, Unbounded) => true
      case (RightUnbounded(lx), RightUnbounded(ly)) => hashB.eqv(lx, ly)
      case (LeftUnbounded(rx), LeftUnbounded(ry)) => hashB.eqv(rx, ry)
      case (Bounded(lx, rx), Bounded(ly, ry)) => hashB.eqv(lx, ly) && hashB.eqv(rx, ry)
      case _ => false
    }
  }
}
