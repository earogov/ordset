package ordset

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

sealed trait Interval[@sp(spNum) +E] {

  def isEmpty: Boolean = false

  def isUnbounded: Boolean = false

  def isBounded: Boolean = false

  def isLeftUnbounded: Boolean = false

  def hasLeftBound: Boolean = false

  def isRightUnbounded: Boolean = false

  def hasRightBound: Boolean = false

  def ->[@sp(Boolean) V](value: V): IntervalMapping[E, V] = IntervalMapping(this, value)

  def cross[E1 >: E](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1]
}

object Interval {

  def apply[E](bound: Bound[E]): Interval[E] = bound match {
    case b: Bound.Lower[E] => RightUnbounded(b)
    case b: Bound.Upper[E] => LeftUnbounded(b)
  }

  def apply[E](lower: Bound.Lower[E], upper: Bound.Upper[E]): Interval[E] = Bounded(lower, upper)

  def empty(): Empty.type = Empty

  def unbounded(): Unbounded.type = Unbounded

  def leftUnbounded[@sp(spNum) E](value: E, isInclusive: Boolean): LeftUnbounded[E] =
    LeftUnbounded(Bound.Upper(value, isInclusive))

  def rightUnbounded[@sp(spNum) E](value: E, isInclusive: Boolean): RightUnbounded[E] =
    RightUnbounded(Bound.Lower(value, isInclusive))

  def bounded[@sp(spNum) E](leftVal: E, leftIncl: Boolean, rightVal: E, rightIncl: Boolean): Bounded[E] =
    Bounded(Bound.Lower(leftVal, leftIncl), Bound.Upper(rightVal, rightIncl))

  implicit def defaultHash[E](implicit hash: Hash[Bound[E]]): Hash[Interval[E]] = new DefaultHash[E]()(hash)

  sealed trait NonEmpty[@sp(spNum) +E] extends Interval[E]

  sealed trait WithLeftBound[@sp(spNum) +E] extends Interval[E] {

    def leftBound: Bound.Lower[E]

    override def hasLeftBound: Boolean = true
  }

  sealed trait WithRightBound[@sp(spNum) +E] extends Interval[E] {

    def rightBound: Bound.Upper[E]

    override def hasRightBound: Boolean = true
  }

  case object Empty extends Interval[Nothing] {

    override def isEmpty: Boolean = true

    override def toString: String = "{}"

    override def cross[E1 >: Nothing](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1] = this
  }

  case object Unbounded extends NonEmpty[Nothing] {

    override def isUnbounded: Boolean = true

    override def toString: String = "x in U"

    override def cross[E1 >: Nothing](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1] = that
  }

  case class RightUnbounded[+E](
    override val leftBound: Bound.Lower[E]
  ) extends NonEmpty[E] with WithLeftBound[E] {

    override def isRightUnbounded: Boolean = true

    override def toString: String = leftBound.toString

    override def cross[E1 >: E](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1] = that match {
      case Empty => that
      case Unbounded => this
      case that: RightUnbounded[E1] =>
        // this:      |--------------
        // that:  |------------------
        if (order.lt(that.leftBound, this.leftBound)) this
        // this:      |--------------
        // that:          |----------
        else that
      case that: WithRightBound[E1] =>
        // this:      |--------------
        // that: ?--|
        if (order.lt(that.rightBound, this.leftBound)) Empty
        else that match {
          // this:      |------------
          // that: ---------|
          case that: LeftUnbounded[E1] => Bounded(this.leftBound, that.rightBound)
          case that: Bounded[E1] =>
            // this:      |----------
            // that:    |---|
            if (order.lt(that.leftBound, this.leftBound)) Bounded(this.leftBound, that.rightBound)
            // this:      |----------
            // that:        |---|
            else that
        }
    }
  }

  case class LeftUnbounded[+E](
    override val rightBound: Bound.Upper[E]
  ) extends NonEmpty[E] with WithRightBound[E] {

    override def isLeftUnbounded: Boolean = true

    override def toString: String = rightBound.toString

    override def cross[E1 >: E](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1] = that match {
      case Empty => that
      case Unbounded => this
      case that: LeftUnbounded[E1] =>
        // this: ---------|
        // that: --------------|
        if (order.lt(this.rightBound, that.rightBound)) this
        // this: ---------|
        // that: -----|
        else that
      case that: WithLeftBound[E1] =>
        // this: ---------|
        // that:            |------?
        if (order.lt(this.rightBound, that.leftBound)) Empty
        else that match {
          // this: ---------|
          // that:      |-----------
          case that: RightUnbounded[E1] => Bounded(that.leftBound, this.rightBound)
          case that: Bounded[E1] =>
            // this: ---------|
            // that:        |---|
            if (order.lt(this.rightBound, that.rightBound)) Bounded(that.leftBound, this.rightBound)
            // this: ---------|
            // that:    |---|
            else that
        }
    }
  }

  case class Bounded[+E](
    override val leftBound: Bound.Lower[E],
    override val rightBound: Bound.Upper[E]
  ) extends NonEmpty[E] with WithLeftBound[E] with WithRightBound[E] {

    override def isBounded: Boolean = true

    override def toString: String = s"$leftBound & $rightBound"

    override def cross[E1 >: E](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1] = that match {
      case Empty => that
      case Unbounded => this
      case that: RightUnbounded[E] =>
        // this:       |-----|
        // that:   |----------------
        if (order.lteqv(that.leftBound, this.leftBound)) this
        // this:       |-----|
        // that:          |---------
        else if (order.lteqv(that.leftBound, this.rightBound)) Bounded(that.leftBound, this.rightBound)
        // this:       |-----|
        // that:                |---
        else Empty
      case that: LeftUnbounded[E] =>
        // this:       |-----|
        // that: ----------------|
        if (order.lteqv(this.rightBound, that.rightBound)) this
        // this:       |-----|
        // that: ---------|
        else if (order.lteqv(this.leftBound, that.rightBound)) Bounded(this.leftBound, that.rightBound)
        // this:       |-----|
        // that: ----|
        else Empty
      case that: Bounded[E] =>
        if (order.lteqv(this.leftBound, that.rightBound))
          if (order.lteqv(that.leftBound, this.leftBound))
            // this:         |-----|
            // that:      |-----|
            if (order.lt(that.rightBound, this.rightBound)) Bounded(this.leftBound, that.rightBound)
            // this:         |-----|
            // that:      |-----------|
            else this
          else if (order.lteqv(that.leftBound, this.rightBound))
            // this:         |-----|
            // that:            |-----|
            if (order.lt(this.rightBound, that.rightBound)) Bounded(that.leftBound, this.rightBound)
            // this:         |-----|
            // that:           |-|
            else that
          // this:         |-----|
          // that:                 |-----|
          else Empty
        // this:              |-----|
        // that:      |-----|
        else Empty
    }
  }

  class DefaultHash[E](implicit hashB: Hash[Bound[E]]) extends Hash[Interval[E]] {
    import util.Hash._

    override def hash(x: Interval[E]): Int = x match {
      case Empty => x.hashCode()
      case Unbounded => x.hashCode()
      case RightUnbounded(l) => product1Hash(hashB.hash(l))
      case LeftUnbounded(r) => product1Hash(hashB.hash(r))
      case Bounded(l, r) => product2Hash(hashB.hash(l), hashB.hash(r))
    }

    override def eqv(x: Interval[E], y: Interval[E]): Boolean = (x, y) match {
      case (Empty, Empty) => true
      case (Unbounded, Unbounded) => true
      case (RightUnbounded(lx), RightUnbounded(ly)) => hashB.eqv(lx, ly)
      case (LeftUnbounded(rx), LeftUnbounded(ry)) => hashB.eqv(rx, ry)
      case (Bounded(lx, rx), Bounded(ly, ry)) => hashB.eqv(lx, ly) && hashB.eqv(rx, ry)
      case _ => false
    }
  }
}
