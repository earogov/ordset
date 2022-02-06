package ordset.core.segmentSeq.set

import ordset.Show
import ordset.core.Bound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.validation.{ValidationPredicate, ValidationException, ValidatingIterable}
import OrderedSetFactoryIterable._

object OrderedSetFactoryIterable {

  /**
   * Returns iterable of upper bounds to construct ordered set.
   * 
   * Iterable provides default validation for ordered sets:
   * <tr>- all bounds must be between bounds of `domain`;</tr>
   * <tr>- sequence of bounds must be monotonically increasing according to `domain` order.</tr>
   */
  def default[E, D[X] <: Domain[X]](
    iterable: IterableOnce[Bound.Upper[E]]
  )(
    implicit domainOps: DomainOps[E, D]
  ): ValidatingIterable[Bound.Upper[E]] =
    new DefaultImpl(iterable, domainOps)
  
  /**
   * Returns iterable with single upper bound to construct ordered set.
   * 
   * Iterable provides validation:
   * <tr>- bound must be between bounds of `domain`.</tr>
   */
  def single[E, D[X] <: Domain[X]](
    bound: Bound.Upper[E]
  )(
    implicit domainOps: DomainOps[E, D]
  ): ValidatingIterable[Bound.Upper[E]] =
    new SingleImpl(bound, domainOps)

  /**
   * Iterable of upper bounds to construct ordered set.
   * 
   * Iterable provides default validation for ordered sets:
   * <tr>- all bounds must be between bounds of `domain`;</tr>
   * <tr>- sequence of bounds must be monotonically increasing according to `domain` order.</tr>
   */
  final class DefaultImpl[E, D[X] <: Domain[X]](
    private val iterable: IterableOnce[Bound.Upper[E]],
    private val domainOps: DomainOps[E, D]
  ) extends ValidatingIterable.ValidatingIterableArity1And2[Bound.Upper[E]](
    iterable,
    new DomainBoundsValidation(domainOps),
    new AdjacentBoundsValidation(domainOps)
  )

  /**
   * Iterable with single upper bound to construct ordered set.
   * 
   * Iterable provides validation:
   * <tr>- bound must be between bounds of `domain`.</tr>
   */
  final class SingleImpl[E, D[X] <: Domain[X]](
    private val bound: Bound.Upper[E],
    private val domainOps: DomainOps[E, D]
  ) extends ValidatingIterable.ValidatingIterableArity1[Bound.Upper[E]](
    List(bound),
    new DomainBoundsValidation(domainOps)
  )

  /**
   * Validation predicate for single bound such that:
   * <tr>- returns `true` if given bound is between bounds of `domain`.</tr>
   */
  final class DomainBoundsValidation[E, D[X] <: Domain[X]](
    private val domainOps: DomainOps[E, D]
  ) extends ValidationPredicate.Arity1[Bound.Upper[E]] {

    override def apply(x: Bound.Upper[E]): Boolean = 
      domainOps.containsBound(x)

    @throws[ValidationException]("if validation is failed")
    override def validate(x: Bound.Upper[E]): Unit = 
      if !apply(x) then {
        val showOps = domainOps.showOps
        val boundStr = showOps.boundShow.show(x)
        val boundsStr = showOps.rangeShow.show(domainOps.boundsRange)
        val causeStr = s"out of domain bounds $boundsStr"
        throw ValidationException.invalidBound(boundStr, causeStr)
      }
  }

  /**
   * Validation predicate for pair of bounds such that:
   * <tr>- returns `true` if first bound is less than second bound according to `domain` order.</tr>
   */
  final class AdjacentBoundsValidation[E, D[X] <: Domain[X]](
    private val domainOps: DomainOps[E, D]
  ) extends ValidationPredicate.Arity2[Bound.Upper[E]] {

    override def apply(prev: Bound.Upper[E], next: Bound.Upper[E]): Boolean = 
      domainOps.boundOrd.lt(prev, next)

    @throws[ValidationException]("if validation is failed")
    override def validate(prev: Bound.Upper[E], next: Bound.Upper[E]): Unit = 
      if !apply(prev, next) then {
        val boundShow = domainOps.showOps.boundShow
        val prevStr = boundShow.show(prev)
        val nextStr = boundShow.show(next)
        val causeStr = s"sequence must be monotonically increasing"
        throw ValidationException.invalidBoundsSeq(prevStr, nextStr, causeStr)
      }
  }
}
