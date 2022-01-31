package ordset.core.domain

import ordset.{Order, Hash}
import ordset.core.{Bound, ExtendedBound}
import ordset.core.range.{SimpleRange, RangeFactory, RangeAlgebra}

trait DomainLike[E] {

  implicit def elementOrd: Order[E] with Hash[E]

  implicit def boundOrd: Bound.DefaultOrder[E]

  implicit def extendedOrd: ExtendedBound.DefaultOrder[E]

  implicit def rangeFactory: RangeFactory[ExtendedBound[E], ExtendedBound[E], SimpleRange]

  implicit def rangeAlgebra: RangeAlgebra[ExtendedBound[E]]

  def boundsRange: SimpleRange[ExtendedBound[E]]

  def lowerBound: ExtendedBound[E] = extendedOrd.lowerBound

  def upperBound: ExtendedBound[E] = extendedOrd.upperBound

  def isContinuous: Boolean

  def isDiscrete: Boolean

  def isBoundedBelow: Boolean

  def isBoundedAbove: Boolean

  def isBounded: Boolean

  def isUnbounded: Boolean

  def containsElement(element: E): Boolean

  def containsBound(bound: Bound[E]): Boolean

  def containsExtended(bound: ExtendedBound[E]): Boolean
}

object DomainLike {

  trait Proxy[E, D[X] <: Domain[X]] extends DomainLike[E] {

    def domain: D[E]

    override implicit def elementOrd: Order[E] with Hash[E] = domain.elementOrd

    override implicit def boundOrd: Bound.DefaultOrder[E] = domain.boundOrd

    override implicit def extendedOrd: ExtendedBound.DefaultOrder[E] = domain.extendedOrd

    override implicit def rangeFactory: RangeFactory[ExtendedBound[E], ExtendedBound[E], SimpleRange] = 
      domain.rangeFactory

    override implicit def rangeAlgebra: RangeAlgebra[ExtendedBound[E]] = domain.rangeAlgebra

    override def boundsRange: SimpleRange[ExtendedBound[E]] = domain.boundsRange

    override def lowerBound: ExtendedBound[E] = domain.lowerBound

    override def upperBound: ExtendedBound[E] = domain.upperBound

    override def isContinuous: Boolean = domain.isContinuous

    override def isDiscrete: Boolean = domain.isDiscrete

    override def isBoundedBelow: Boolean = domain.isBoundedBelow

    override def isBoundedAbove: Boolean = domain.isBoundedAbove

    override def isBounded: Boolean = domain.isBounded

    override def isUnbounded: Boolean = domain.isUnbounded

    override def containsElement(element: E): Boolean = domain.containsElement(element)

    override def containsBound(bound: Bound[E]): Boolean = domain.containsBound(bound)

    override def containsExtended(bound: ExtendedBound[E]): Boolean = domain.containsExtended(bound)
  }

  final case class ProxyImpl[E, D[X] <: Domain[X]](override val domain: D[E]) extends Proxy[E, D]
}