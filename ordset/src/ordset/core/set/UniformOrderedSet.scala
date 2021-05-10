package ordset.core.set

import ordset.core.{AbstractUniformSegmentSeq, Bound, SegmentSeq, TreapSegmentSeq}
import ordset.core.domain.{Domain, DomainOps, OrderValidationFunc}
import ordset.random.RngManager

class UniformOrderedSet[E, D <: Domain[E]](
  final override val value: Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractUniformSegmentSeq[E, D, Boolean]
  with OrderedSetCommons[E, D] {

  // Transformation ----------------------------------------------------------- //

  // Protected section -------------------------------------------------------- //  
  @inline
  protected final override def isValueIncluded(value: Boolean): Boolean = value

  protected final override def consPrepended(bound: Bound[E], firstValue: Boolean): OrderedSet[E, D] =
    if (valueOps.eqv(firstValue, value))
      this
    else
      TreapOrderedSet.fromIterableUnsafe(
        List(bound.provideUpper), firstValue, domainOps
      )(
        OrderValidationFunc.alwaysTrue
      )(
        rngManager
      )
  
  protected final override def consAppended(bound: Bound[E], lastValue: Boolean): OrderedSet[E, D] =
    if (valueOps.eqv(value, lastValue)) 
      this
    else
      TreapOrderedSet.fromIterableUnsafe(
        List(bound.provideUpper), value, domainOps
      )(
        OrderValidationFunc.alwaysTrue
      )(
        rngManager
      )
}

object UniformOrderedSet {

  def apply[E, D <: Domain[E]](
    value: Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](value)

  def empty[E, D <: Domain[E]](
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](false)

  def universal[E, D <: Domain[E]](
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](true)
}
