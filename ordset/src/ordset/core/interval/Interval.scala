package ordset.core.interval

import ordset.core.{Bound, ExtendedBound, SetBuilderFormat}
import ordset.core.domain.Domain
import ordset.{Hash, Show, util}

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

sealed trait Interval[@sp(spNum) E, D <: Domain[E]] {

  def domain: D

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
    implicit 
    boundHash: Hash[Bound[E]], 
    domainHash: Hash[D]
  ): Hash[Interval[E, D]] =
    new DefaultHash()(boundHash, domainHash)

  implicit def defaultShow[E, D <: Domain[E]](
    implicit elementShow: Show[E]
  ): Show[Interval[E, D]] =
    SetBuilderFormat.intervalShow(elementShow)

  sealed trait NonEmpty[E, D <: Domain[E]] extends Interval[E, D] {

    def lowerExtended: ExtendedBound.Lower[E] = ExtendedBound.BelowAll

    def upperExtended: ExtendedBound.Upper[E] = ExtendedBound.AboveAll
  }

  sealed trait WithLowerBound[@sp(spNum) E, D <: Domain[E]] extends NonEmpty[E, D] {

    def lowerBound: Bound.Lower[E]

    override def hasLowerBound: Boolean = true

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = domain.boundOrd.eqv(lowerBound, bound)

    override def lowerExtended: ExtendedBound.Lower[E] = lowerBound
  }

  sealed trait WithUpperBound[@sp(spNum) E, D <: Domain[E]] extends NonEmpty[E, D] {

    def upperBound: Bound.Upper[E]

    override def hasUpperBound: Boolean = true

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = domain.boundOrd.eqv(upperBound, bound)

    override def upperExtended: ExtendedBound.Upper[E] = upperBound
  }

  final case class Empty[E, D <: Domain[E]](
  )(
    implicit override val domain: D
  ) extends Interval[E, D] {

    override def isEmpty: Boolean = true

    override def toString: String = SetBuilderFormat.emptyInterval
  }

  final case class Universal[E, D <: Domain[E]](
  )(
    implicit override val domain: D
  ) extends NonEmpty[E, D] {

    override def isUniversal: Boolean = true

    override def toString: String = SetBuilderFormat.universalInterval
  }

  final case class Greater[@sp(spNum) E, D <: Domain[E]](
    override val lowerBound: Bound.Lower[E]
  )(
    implicit override val domain: D
  ) extends WithLowerBound[E, D] {

    override def toString: String = SetBuilderFormat.lowerBoundedInterval(this, SetBuilderFormat.toStringFunc[E])
  }

  final case class Less[@sp(spNum) E, D <: Domain[E]](
    override val upperBound: Bound.Upper[E]
  )(
    implicit override val domain: D
  ) extends WithUpperBound[E, D] {

    override def toString: String = SetBuilderFormat.upperBoundedInterval(this, SetBuilderFormat.toStringFunc[E])
  }

  final case class Between[@sp(spNum) E, D <: Domain[E]](
    override val lowerBound: Bound.Lower[E],
    override val upperBound: Bound.Upper[E]
  )(
    implicit override val domain: D
  ) extends WithLowerBound[E, D] with WithUpperBound[E, D] {

    override def toString: String = SetBuilderFormat.boundedInterval(this, SetBuilderFormat.toStringFunc[E])
  }

  final class DefaultHash[E, D <: Domain[E]]()(
    implicit 
    boundHash: Hash[Bound[E]], 
    domainHash: Hash[D]
  ) extends Hash[Interval[E, D]] {

    import util.HashUtil._

    override def hash(x: Interval[E, D]): Int = x match {
      case x: Empty[e, d] => 
        product1Hash(domainHash.hash(x.domain))
      case x: Universal[e, d] => 
        product1Hash(domainHash.hash(x.domain))
      case x: Greater[e, d] => 
        product2Hash(boundHash.hash(x.lowerBound), domainHash.hash(x.domain))
      case x: Less[e, d] => 
        product2Hash(boundHash.hash(x.upperBound), domainHash.hash(x.domain))
      case x: Between[e, d] => 
        product3Hash(boundHash.hash(x.lowerBound), boundHash.hash(x.upperBound), domainHash.hash(x.domain))
    }

    override def eqv(x: Interval[E, D], y: Interval[E, D]): Boolean =
      if (domainHash.eqv(x.domain, y.domain)) x match {
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
