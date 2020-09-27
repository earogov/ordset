package ordset

import ordset.domain.{Domain, DomainOps}

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

sealed trait Interval[@sp(spNum) E, D <: Domain[E]] {

  def domainOps: DomainOps[E, D]

  def isEmpty: Boolean = false

  def isUniversal: Boolean = false

  def hasLowerBound: Boolean = false

  def hasUpperBound: Boolean = false

  def ->[@sp(Boolean) V](value: V): IntervalMapping[E, D, V] = IntervalMapping(this, value)

//  def cross[E1 >: E](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1]
}

object Interval {

  implicit def defaultHash[E, D <: Domain[E]](
    implicit boundHash: Hash[Bound[E]], domainHash: Hash[D]): Hash[Interval[E, D]] =
    new DefaultHash()(boundHash, domainHash)

  sealed trait NonEmpty[E, D <: Domain[E]] extends Interval[E, D]

  sealed trait WithLowerBound[@sp(spNum) E, D <: Domain[E]] extends Interval[E, D] {

    def lowerBound: Bound.Lower[E]

    override def hasLowerBound: Boolean = true
  }

  sealed trait WithUpperBound[@sp(spNum) E, D <: Domain[E]] extends Interval[E, D] {

    def upperBound: Bound.Upper[E]

    override def hasUpperBound: Boolean = true
  }

  case class Empty[E, D <: Domain[E]](
    override val domainOps: DomainOps[E, D]
  ) extends Interval[E, D] {

    override def isEmpty: Boolean = true

    override def toString: String = "{}"

//    override def cross[E1 >: Nothing](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1] = this
  }

  case class Universal[E, D <: Domain[E]](
    override val domainOps: DomainOps[E, D]
  ) extends NonEmpty[E, D] {

    override def isUniversal: Boolean = true

    override def toString: String = "x in U"

//    override def cross[E1 >: Nothing](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1] = that
  }

  case class Greater[@sp(spNum) E, D <: Domain[E]](
    override val lowerBound: Bound.Lower[E]
  )(
    override val domainOps: DomainOps[E, D]
  ) extends NonEmpty[E, D] with WithLowerBound[E, D] {

    override def toString: String = lowerBound.toString

//    override def cross[E1 >: E](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1] = that match {
//      case Empty => that
//      case Unbounded => this
//      case that: RightUnbounded[E1] =>
//        // this:      |--------------
//        // that:  |------------------
//        if (order.lt(that.leftBound, this.leftBound)) this
//        // this:      |--------------
//        // that:          |----------
//        else that
//      case that: WithRightBound[E1] =>
//        // this:      |--------------
//        // that: ?--|
//        if (order.lt(that.rightBound, this.leftBound)) Empty
//        else that match {
//          // this:      |------------
//          // that: ---------|
//          case that: LeftUnbounded[E1] => Bounded(this.leftBound, that.rightBound)
//          case that: Bounded[E1] =>
//            // this:      |----------
//            // that:    |---|
//            if (order.lt(that.leftBound, this.leftBound)) Bounded(this.leftBound, that.rightBound)
//            // this:      |----------
//            // that:        |---|
//            else that
//        }
//    }
  }

  case class Less[@sp(spNum) E, D <: Domain[E]](
    override val upperBound: Bound.Upper[E]
  )(
    override val domainOps: DomainOps[E, D]
  ) extends NonEmpty[E, D] with WithUpperBound[E, D] {

    override def toString: String = upperBound.toString

//    override def cross[E1 >: E](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1] = that match {
//      case Empty => that
//      case Unbounded => this
//      case that: LeftUnbounded[E1] =>
//        // this: ---------|
//        // that: --------------|
//        if (order.lt(this.rightBound, that.rightBound)) this
//        // this: ---------|
//        // that: -----|
//        else that
//      case that: WithLeftBound[E1] =>
//        // this: ---------|
//        // that:            |------?
//        if (order.lt(this.rightBound, that.leftBound)) Empty
//        else that match {
//          // this: ---------|
//          // that:      |-----------
//          case that: RightUnbounded[E1] => Bounded(that.leftBound, this.rightBound)
//          case that: Bounded[E1] =>
//            // this: ---------|
//            // that:        |---|
//            if (order.lt(this.rightBound, that.rightBound)) Bounded(that.leftBound, this.rightBound)
//            // this: ---------|
//            // that:    |---|
//            else that
//        }
//    }
  }

  case class Between[@sp(spNum) E, D <: Domain[E]](
    override val lowerBound: Bound.Lower[E],
    override val upperBound: Bound.Upper[E]
  )(
    override val domainOps: DomainOps[E, D]
  ) extends NonEmpty[E, D] with WithLowerBound[E, D] with WithUpperBound[E, D] {

    override def toString: String = s"$lowerBound & $upperBound"

//    override def cross[E1 >: E](that: Interval[E1])(implicit order: Order[Bound[E1]]): Interval[E1] = that match {
//      case Empty => that
//      case Unbounded => this
//      case that: RightUnbounded[E] =>
//        // this:       |-----|
//        // that:   |----------------
//        if (order.lteqv(that.leftBound, this.leftBound)) this
//        // this:       |-----|
//        // that:          |---------
//        else if (order.lteqv(that.leftBound, this.rightBound)) Bounded(that.leftBound, this.rightBound)
//        // this:       |-----|
//        // that:                |---
//        else Empty
//      case that: LeftUnbounded[E] =>
//        // this:       |-----|
//        // that: ----------------|
//        if (order.lteqv(this.rightBound, that.rightBound)) this
//        // this:       |-----|
//        // that: ---------|
//        else if (order.lteqv(this.leftBound, that.rightBound)) Bounded(this.leftBound, that.rightBound)
//        // this:       |-----|
//        // that: ----|
//        else Empty
//      case that: Bounded[E] =>
//        if (order.lteqv(this.leftBound, that.rightBound))
//          if (order.lteqv(that.leftBound, this.leftBound))
//            // this:         |-----|
//            // that:      |-----|
//            if (order.lt(that.rightBound, this.rightBound)) Bounded(this.leftBound, that.rightBound)
//            // this:         |-----|
//            // that:      |-----------|
//            else this
//          else if (order.lteqv(that.leftBound, this.rightBound))
//            // this:         |-----|
//            // that:            |-----|
//            if (order.lt(this.rightBound, that.rightBound)) Bounded(that.leftBound, this.rightBound)
//            // this:         |-----|
//            // that:           |-|
//            else that
//          // this:         |-----|
//          // that:                 |-----|
//          else Empty
//        // this:              |-----|
//        // that:      |-----|
//        else Empty
//    }
  }

  class DefaultHash[E, D <: Domain[E]]()(
    implicit boundHash: Hash[Bound[E]], domainHash: Hash[D]
  ) extends Hash[Interval[E, D]] {

    import util.Hash._

    override def hash(x: Interval[E, D]): Int = x match {
      case x: Empty[E, D]     => product1Hash(domainHash.hash(x.domainOps.domain))
      case x: Universal[E, D] => product1Hash(domainHash.hash(x.domainOps.domain))
      case Greater(l)         => product2Hash(boundHash.hash(l), domainHash.hash(x.domainOps.domain))
      case Less(r)            => product2Hash(boundHash.hash(r), domainHash.hash(x.domainOps.domain))
      case Between(l, r)      => product3Hash(boundHash.hash(l), boundHash.hash(r), domainHash.hash(x.domainOps.domain))
    }

    override def eqv(x: Interval[E, D], y: Interval[E, D]): Boolean =
      if (domainHash.eqv(x.domainOps.domain, y.domainOps.domain)) (x, y) match {
        case (_: Empty[E, D], _: Empty[E, D])         => true
        case (_: Universal[E, D], _: Universal[E, D]) => true
        case (Greater(lx), Greater(ly))               => boundHash.eqv(lx, ly)
        case (Less(rx), Less(ry))                     => boundHash.eqv(rx, ry)
        case (Between(lx, rx), Between(ly, ry))       => boundHash.eqv(lx, ly) && boundHash.eqv(rx, ry)
        case _                                        => false
      }
      else false
  }
}
