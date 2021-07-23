package ordset.core.set

import ordset.core.AbstractUniformSegmentSeq.UniformSingleSegment
import ordset.core.{AbstractUniformSegmentSeq, Bound, SegmentSeq, SeqValidationPredicate, TreapSegmentSeq}
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager

class UniformOrderedSet[E, D <: Domain[E]] protected (
  final override val value: Boolean,
  final val setFactory: OrderedSetFactory[E, D, OrderedSet[E, D]]
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractUniformSegmentSeq[E, D, Boolean]
  with OrderedSetCommons[E, D, UniformSingleSegment[E, D, Boolean]] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: Boolean): UniformOrderedSet[E, D] =
    if (valueOps.eqv(this.value, value)) this
    else UniformOrderedSet.apply(value, setFactory)

  protected final override def consPrepended(bound: Bound[E], firstValue: Boolean): OrderedSet[E, D] =
    if (valueOps.eqv(firstValue, value))
      this
    else
      setFactory.unsafeBuildAsc(
        List(bound.provideUpper), firstValue, domainOps
      )(
        SeqValidationPredicate.alwaysTrue
      )(
        rngManager
      )
  
  protected final override def consAppended(bound: Bound[E], lastValue: Boolean): OrderedSet[E, D] =
    if (valueOps.eqv(value, lastValue)) 
      this
    else
      setFactory.unsafeBuildAsc(
        List(bound.provideUpper), value, domainOps
      )(
        SeqValidationPredicate.alwaysTrue
      )(
        rngManager
      )
}

object UniformOrderedSet {

  def apply[E, D <: Domain[E]](
    value: Boolean,
    setFactory: OrderedSetFactory[E, D, OrderedSet[E, D]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet(value, setFactory)

  def empty[E, D <: Domain[E]](
    setFactory: OrderedSetFactory[E, D, OrderedSet[E, D]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet(false, setFactory)

  def universal[E, D <: Domain[E]](
    setFactory: OrderedSetFactory[E, D, OrderedSet[E, D]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet(true, setFactory)

  def default[E, D <: Domain[E]](
    value: Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet(value, TreapOrderedSet.getFactory)

  def defaultEmpty[E, D <: Domain[E]](
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet(false, TreapOrderedSet.getFactory)

  def defaultUniversal[E, D <: Domain[E]](
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet(true, TreapOrderedSet.getFactory)
}
