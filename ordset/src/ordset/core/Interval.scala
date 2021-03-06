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

  final case class Empty[E, D <: Domain[E]](
  )(
    implicit override val domainOps: DomainOps[E, D]
  ) extends Interval[E, D] {

    override def isEmpty: Boolean = true

    override def toString: String = SetBuilderFormat.emptyInterval
  }

  final case class Universal[E, D <: Domain[E]](
  )(
    implicit override val domainOps: DomainOps[E, D]
  ) extends NonEmpty[E, D] {

    override def isUniversal: Boolean = true

    override def toString: String = SetBuilderFormat.universalInterval
  }

  final case class Greater[@sp(spNum) E, D <: Domain[E]](
    override val lowerBound: Bound.Lower[E]
  )(
    implicit override val domainOps: DomainOps[E, D]
  ) extends WithLowerBound[E, D] {

    override def toString: String = SetBuilderFormat.lowerBoundedInterval(this, (e: E) => e.toString)
  }

  final case class Less[@sp(spNum) E, D <: Domain[E]](
    override val upperBound: Bound.Upper[E]
  )(
    implicit override val domainOps: DomainOps[E, D]
  ) extends WithUpperBound[E, D] {

    override def toString: String = SetBuilderFormat.upperBoundedInterval(this, (e: E) => e.toString)
  }

  final case class Between[@sp(spNum) E, D <: Domain[E]](
    override val lowerBound: Bound.Lower[E],
    override val upperBound: Bound.Upper[E]
  )(
    implicit override val domainOps: DomainOps[E, D]
  ) extends WithLowerBound[E, D] with WithUpperBound[E, D] {

    override def toString: String = SetBuilderFormat.boundedInterval(this, (e: E) => e.toString)
  }

  final class DefaultHash[E, D <: Domain[E]]()(
    implicit boundHash: Hash[Bound[E]], domainHash: Hash[D]
  ) extends Hash[Interval[E, D]] {

    import util.HashUtil._

    override def hash(x: Interval[E, D]): Int = x match {
      case x: Empty[e, d] => 
        product1Hash(domainHash.hash(x.domainOps.domain))
      case x: Universal[e, d] => 
        product1Hash(domainHash.hash(x.domainOps.domain))
      case x: Greater[e, d] => 
        product2Hash(boundHash.hash(x.lowerBound), domainHash.hash(x.domainOps.domain))
      case x: Less[e, d] => 
        product2Hash(boundHash.hash(x.upperBound), domainHash.hash(x.domainOps.domain))
      case x: Between[e, d] => 
        product3Hash(boundHash.hash(x.lowerBound), boundHash.hash(x.upperBound), domainHash.hash(x.domainOps.domain))
    }

    override def eqv(x: Interval[E, D], y: Interval[E, D]): Boolean =
      if (domainHash.eqv(x.domainOps.domain, y.domainOps.domain)) x match {
        case x: Empty[e, d] => y match {
          case _: Empty[e, d] => true
          case _ => false
        }
        case x: Universal[e, d] => y match {
          case _: Universal[e, d] => true
          case _ => false
        }
        case x: Greater[e, d] => y match {
          case y: Greater[e, d] => boundHash.eqv(x.lowerBound: Bound.Lower[E], y.lowerBound)
          case _ => false
        }
        case x: Less[e, d] => y match {
          case y: Less[e, d] => boundHash.eqv(x.upperBound, y.upperBound)
          case _ => false
        }
        case x: Between[e, d] => y match {
          case y: Between[e, d] => 
            boundHash.eqv(x.lowerBound, y.lowerBound) && boundHash.eqv(x.upperBound, y.upperBound)
          case _ => false
        }
      }
      else false
  }
}
