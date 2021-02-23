package ordset.core

import ordset.core.domain.{Domain, DomainOps}
import ordset.{Hash, Show, util}

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

sealed trait Interval[@sp(spNum) E, D <: Domain[E]] {

  def domainOps: DomainOps[E, D]

  def isEmpty: Boolean = false

  def isUniversal: Boolean = false

  def hasLowerBound: Boolean = false

  def hasUpperBound: Boolean = false

  def hasLowerBound(bound: Bound.Lower[E]): Boolean = false

  def hasUpperBound(bound: Bound.Upper[E]): Boolean = false

  def ->[@sp(Boolean) V](value: V): IntervalRelation[E, D, V] = IntervalRelation(this, value)
}

object Interval {

  implicit def defaultHash[E, D <: Domain[E]](
    implicit boundHash: Hash[Bound[E]], domainHash: Hash[D]
  ): Hash[Interval[E, D]] =
    new DefaultHash()(boundHash, domainHash)

  implicit def defaultShow[E, D <: Domain[E]](
    implicit elementShow: Show[E]
  ): Show[Interval[E, D]] =
    SetBuilderFormat.intervalShow(elementShow)

  sealed trait NonEmpty[E, D <: Domain[E]] extends Interval[E, D]

  sealed trait WithLowerBound[@sp(spNum) E, D <: Domain[E]] extends NonEmpty[E, D] {

    def lowerBound: Bound.Lower[E]

    override def hasLowerBound: Boolean = true

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = domainOps.boundOrd.eqv(lowerBound, bound)
  }

  sealed trait WithUpperBound[@sp(spNum) E, D <: Domain[E]] extends NonEmpty[E, D] {

    def upperBound: Bound.Upper[E]

    override def hasUpperBound: Boolean = true

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = domainOps.boundOrd.eqv(upperBound, bound)
  }

  case class Empty[E, D <: Domain[E]](
    override val domainOps: DomainOps[E, D]
  ) extends Interval[E, D] {

    override def isEmpty: Boolean = true

    override def toString: String = SetBuilderFormat.emptyInterval
  }

  case class Universal[E, D <: Domain[E]](
    override val domainOps: DomainOps[E, D]
  ) extends NonEmpty[E, D] {

    override def isUniversal: Boolean = true

    override def toString: String = SetBuilderFormat.universalInterval
  }

  case class Greater[@sp(spNum) E, D <: Domain[E]](
    override val lowerBound: Bound.Lower[E]
  )(
    override val domainOps: DomainOps[E, D]
  ) extends WithLowerBound[E, D] {

    override def toString: String = SetBuilderFormat.lowerBoundedInterval(this, (e: E) => e.toString)
  }

  case class Less[@sp(spNum) E, D <: Domain[E]](
    override val upperBound: Bound.Upper[E]
  )(
    override val domainOps: DomainOps[E, D]
  ) extends WithUpperBound[E, D] {

    override def toString: String = SetBuilderFormat.upperBoundedInterval(this, (e: E) => e.toString)
  }

  case class Between[@sp(spNum) E, D <: Domain[E]](
    override val lowerBound: Bound.Lower[E],
    override val upperBound: Bound.Upper[E]
  )(
    override val domainOps: DomainOps[E, D]
  ) extends WithLowerBound[E, D] with WithUpperBound[E, D] {

    override def toString: String = SetBuilderFormat.boundedInterval(this, (e: E) => e.toString)
  }

  final class DefaultHash[E, D <: Domain[E]]()(
    implicit boundHash: Hash[Bound[E]], domainHash: Hash[D]
  ) extends Hash[Interval[E, D]] {

    import util.HashUtil._

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