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
   * Iterable provides default validation for ordered sets according to domain order:
   *
   * <div>1. For each bound `b` must be satisfied condition:      §§           </div>
   * <div>   (`b` `≥` domain lower bound) and (`b` `<` domain upper bound).  </div>
   * 
   * <div>2. Sequence of bounds must be monotonically increasing.</div>
   */
  def default[E, D[X] <: Domain[X]](
    iterable: Iterable[Bound.Upper[E]]
  )(
    implicit domainOps: DomainOps[E, D]
  ): Default[E, D] =
    new Default(iterable, domainOps)
  
  /**
   * Returns iterable with single upper bound to construct ordered set.
   * 
   * Iterable provides validation according to domain order:
   * 
   * <div>1. For each bound `b` must be satisfied condition:                 </div>
   * <div>   (`b` `≥` domain lower bound) and (`b` `<` domain upper bound).  </div>
   */
  def single[E, D[X] <: Domain[X]](
    bound: Bound.Upper[E]
  )(
    implicit domainOps: DomainOps[E, D]
  ): Single[E, D] =
    new Single(bound, domainOps)

  /**
   * Iterable of upper bounds to construct ordered set.
   * 
   * Iterable provides default validation for ordered sets according to domain order:
   *  
   * <div>1. For each bound `b` must be satisfied condition:                 </div>
   * <div>   (`b` `≥` domain lower bound) and (`b` `<` domain upper bound).  </div>
   * 
   * <div>2. Sequence of bounds must be monotonically increasing.</div>
   */
  final class Default[E, D[X] <: Domain[X]](
    private val iterable: Iterable[Bound.Upper[E]],
    private val domainOps: DomainOps[E, D]
  ) extends ValidatingIterable.ValidatingIterableArity1And2[Bound.Upper[E]](
    iterable,
    new DomainBoundsValidation(domainOps),
    new AdjacentBoundsValidation(domainOps)
  )

  /**
   * Iterable with single upper bound to construct ordered set.
   * 
   * Iterable provides validation according to domain order:
  *
   * <div>1. For each bound `b` must be satisfied condition:                 </div>
   * <div>   (`b` `≥` domain lower bound) and (`b` `<` domain upper bound).  </div>
   */
  final class Single[E, D[X] <: Domain[X]](
    private val bound: Bound.Upper[E],
    private val domainOps: DomainOps[E, D]
  ) extends ValidatingIterable.ValidatingIterableArity1[Bound.Upper[E]](
    List(bound),
    new DomainBoundsValidation(domainOps)
  )

  /**
   * Validation predicate for single bound `b`. Returns `true`, iff:
   *  
   * <div>1. (`b` `≥` domain lower bound) and (`b` `<` domain upper bound) according to domain order.</div>
   */
  final class DomainBoundsValidation[E, D[X] <: Domain[X]](
    private val domainOps: DomainOps[E, D]
  ) extends ValidationPredicate.Arity1[Bound.Upper[E]] {

    override def apply(x: Bound.Upper[E]): Boolean = {
      val ord = domainOps.extendedOrd
      ord.lteqv(domainOps.lowerBound, x) && ord.lt(x, domainOps.upperBound)
    }

    @throws[ValidationException]("if validation is failed")
    override def validate(x: Bound.Upper[E], index: Long): Unit = 
      if !apply(x) then {
        val showOps = domainOps.showOps
        val boundStr = showOps.extendedShow.show(x)
        val causeStr = 
          if (domainOps.extendedOrd.eqv(x, domainOps.upperBound)) {
            "bound must be less than upper bound of domain"
          } else {
            s"out of domain bounds ${showOps.rangeShow.show(domainOps.boundsRange)}"
          }
        throw ValidationException.invalidBound(boundStr, index, causeStr)
      }
  }

  /**
   * Validation predicate for pair of bounds (`prev`, `next`). Returns `true`, iff:
   *  
   * <div>1. (`prev` `<` `next`) according to domain order.</div>
   */
  final class AdjacentBoundsValidation[E, D[X] <: Domain[X]](
    private val domainOps: DomainOps[E, D]
  ) extends ValidationPredicate.Arity2[Bound.Upper[E]] {

    override def apply(prev: Bound.Upper[E], next: Bound.Upper[E]): Boolean = 
      domainOps.boundOrd.lt(prev, next)

    @throws[ValidationException]("if validation is failed")
    override def validate(prev: Bound.Upper[E], next: Bound.Upper[E], index: Long): Unit = 
      if !apply(prev, next) then {
        val boundShow = domainOps.showOps.boundShow
        val prevStr = boundShow.show(prev)
        val nextStr = boundShow.show(next)
        val causeStr = "sequence must be monotonically increasing"
        throw ValidationException.invalidBoundsSeq(prevStr, nextStr, index, causeStr)
      }
  }
}
