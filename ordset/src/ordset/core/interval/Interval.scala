package ordset.core.interval

import ordset.core.{Bound, ExtendedBound, SetBuilderFormat}
import ordset.core.domain.Domain
import ordset.{Hash, Show, util}

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

sealed trait Interval[@sp(spNum) E, D <: Domain[E]] {

  /** Domain of interval. */
  def domain: D

  /** @return `true` if interval is empty, i.e. contains no elements. */
  def isEmpty: Boolean = false

  /** @return `true` if interval is universal, i.e. contains all elements of domain. */
  def isUniversal: Boolean = false

  /** @return `true` if `bound` is between interval bounds. */
  def containsBound(bound: Bound[E]): Boolean

  /** @return `true` if `bound` is between interval bounds. */
  def containsExtended(bound: ExtendedBound[E]): Boolean

  /** @return `true` if `element` is between interval bounds. */
  def containsElement(element: E): Boolean = containsBound(Bound.Upper.including(element))

  /** @return `true` if interval has limited lower bound. */
  def hasLowerBound: Boolean = false

  /** @return `true` if interval has specified limited lower bound. */
  def hasLowerBound(bound: Bound.Lower[E]): Boolean = false

  /** @return `true` if interval has specified extended lower bound. */
  def hasLowerExtended(bound: ExtendedBound.Lower[E]): Boolean =
    bound match {
      case b: Bound.Lower[E] => hasLowerBound(b)
      case ExtendedBound.BelowAll => !hasLowerBound && !isEmpty
    }

  /** @return `true` if interval has limited upper bound. */
  def hasUpperBound: Boolean = false

  /** @return `true` if interval has specified limited upper bound. */
  def hasUpperBound(bound: Bound.Upper[E]): Boolean = false

  /** @return `true` if interval has specified extended upper bound. */
  def hasUpperExtended(bound: ExtendedBound.Upper[E]): Boolean =
    bound match {
      case b: Bound.Upper[E] => hasUpperBound(b)
      case ExtendedBound.AboveAll => !hasUpperBound && !isEmpty
    }

  /** @return [[IntervalRelation]] for current interval and specified `value`. */
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

    /**
     * Get extended lower bound of interval:
     * <tr>- if interval doesn't have limited lower bound, returns [[ExtendedBound.BelowAll]];</tr>
     * <tr>- otherwise returns limited lower bound.</tr>
     */
    def lowerExtended: ExtendedBound.Lower[E] = ExtendedBound.BelowAll

    /**
     * Get extended upper bound of interval:
     * <tr>- if interval doesn't have limited upper bound, returns [[ExtendedBound.AboveAll]];</tr>
     * <tr>- otherwise returns limited upper bound.</tr>
     */
    def upperExtended: ExtendedBound.Upper[E] = ExtendedBound.AboveAll

    /**
     * If `bound` is outside of interval, returns closest bound of interval (either lower or upper).
     * Otherwise returns `bound`.
     *
     * <h3>Note</h3>
     *
     * The formula below seems to be equivalent to method definition:
     *
     * output bound = min(max(`bound`, lower bound of interval), upper bound of interval)     (1)
     *
     * But there is a subtle difference: according to bound ordering defined by domain two bounds,
     * for example, 5`]` and `[`5 are equal. So min() and max() operators can return any of them.
     *
     * Consider the case.
     * {{{
     *       bound
     *         ]
     *         [-----------)
     *         5     ^     10
     *            interval
     * }}}
     * [[restrictBound]] must return `bound` = 5`]`.
     * But implementation based on formula (1) can return either 5`]` or 5`[`.
     */
    def restrictBound(bound: Bound[E]): Bound[E]

    /**
     * If `bound` is outside of interval, returns closest bound of interval (either lower or upper).
     * Otherwise returns `bound`.
     *
     * @see [[restrictBound]]
     */
    def restrictExtended(bound: ExtendedBound[E]): ExtendedBound[E] =
      bound match {
        case b: Bound[E] => restrictBound(b)
        case ExtendedBound.BelowAll => lowerExtended
        case ExtendedBound.AboveAll => upperExtended
      }
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

    override def containsBound(bound: Bound[E]): Boolean = false

    override def containsExtended(bound: ExtendedBound[E]): Boolean = false

    override def toString: String = SetBuilderFormat.emptyInterval
  }

  final case class Universal[E, D <: Domain[E]](
  )(
    implicit override val domain: D
  ) extends NonEmpty[E, D] {

    override def isUniversal: Boolean = true

    override def containsBound(bound: Bound[E]): Boolean = true

    override def containsExtended(bound: ExtendedBound[E]): Boolean = true

    override def restrictBound(bound: Bound[E]): Bound[E] = bound

    override def restrictExtended(bound: ExtendedBound[E]): ExtendedBound[E] = bound

    override def toString: String = SetBuilderFormat.universalInterval
  }

  final case class Greater[@sp(spNum) E, D <: Domain[E]](
    override val lowerBound: Bound.Lower[E]
  )(
    implicit override val domain: D
  ) extends WithLowerBound[E, D] {

    override def containsBound(bound: Bound[E]): Boolean = domain.boundOrd.lteqv(lowerBound, bound)

    override def containsExtended(bound: ExtendedBound[E]): Boolean =
      bound match {
        case b: Bound[E] => containsBound(b)
        case ExtendedBound.BelowAll => false
        case ExtendedBound.AboveAll => true
      }

    override def restrictBound(bound: Bound[E]): Bound[E] =
      if (domain.boundOrd.lt(bound, lowerBound)) lowerBound
      else bound

    override def toString: String = SetBuilderFormat.lowerBoundedInterval(this, SetBuilderFormat.toStringFunc[E])
  }

  final case class Less[@sp(spNum) E, D <: Domain[E]](
    override val upperBound: Bound.Upper[E]
  )(
    implicit override val domain: D
  ) extends WithUpperBound[E, D] {

    override def containsBound(bound: Bound[E]): Boolean = domain.boundOrd.gteqv(upperBound, bound)

    override def containsExtended(bound: ExtendedBound[E]): Boolean =
      bound match {
        case b: Bound[E] => containsBound(b)
        case ExtendedBound.BelowAll => true
        case ExtendedBound.AboveAll => false
      }

    override def restrictBound(bound: Bound[E]): Bound[E] =
      if (domain.boundOrd.gt(bound, upperBound)) upperBound
      else bound

    override def toString: String = SetBuilderFormat.upperBoundedInterval(this, SetBuilderFormat.toStringFunc[E])
  }

  final case class Between[@sp(spNum) E, D <: Domain[E]](
    override val lowerBound: Bound.Lower[E],
    override val upperBound: Bound.Upper[E]
  )(
    implicit override val domain: D
  ) extends WithLowerBound[E, D] with WithUpperBound[E, D] {

    override def containsBound(bound: Bound[E]): Boolean = {
      val boundOrd = domain.boundOrd
      boundOrd.lteqv(lowerBound, bound) && boundOrd.gteqv(upperBound, bound)
    }

    override def containsExtended(bound: ExtendedBound[E]): Boolean =
      bound match {
        case b: Bound[E] => containsBound(b)
        case _ => false
      }

    override def restrictBound(bound: Bound[E]): Bound[E] = {
      val boundOrd = domain.boundOrd
      if (boundOrd.lt(bound, lowerBound)) lowerBound
      else if (boundOrd.gt(bound, upperBound)) upperBound
      else bound
    }

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
