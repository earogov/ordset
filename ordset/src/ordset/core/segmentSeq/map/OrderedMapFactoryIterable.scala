package ordset.core.segmentSeq.map

import ordset.core.ExtendedBound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.segmentSeq.validation.{ValidationPredicate, ValidationException, ValidatingIterable}
import OrderedMapFactoryIterable._

object OrderedMapFactoryIterable {
  
  /**
   * Returns iterable of tuples (upper bound, value) to construct ordered map.
   * 
   * Iterable provides default validation for ordered maps:
   *
   * <div>1. For each bound `b` must be satisfied at least one of the conditions according to domain order:  </div>
   * <div>1.a. (`b` `≥` domain lower bound) and (`b` `<` domain upper bound).                                </div>
   * <div>1.b. `b` `==` [[ExtendedBound.AboveAll]] - for last bound of iterable.                             </div>
   * 
   * <div>2. Sequence of bounds must be monotonically increasing according to domain order.                  </div>
   * 
   * <div>3. Values associated with adjacent bounds must be different according to `valueOps` order.         </div>
   */
  def default[E, D[X] <: Domain[X], V](
    iterable: Iterable[BoundValue[E, V]]
  )(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  ): Default[E, D, V] = 
    new Default(iterable, domainOps, valueOps)

  /**
   * Returns iterable with single tuple ([[ExtendedBound.AboveAll]], value) to construct ordered map.
   */
  def single[E, D[X] <: Domain[X], V](value: V): Single[E, D, V] =
    new Single(value)

  /**
   * Iterable of tuples (upper bound, value) to construct ordered map.
   * 
   * Iterable provides default validation for ordered maps:
   *
   * <div>1. For each bound `b` must be satisfied at least one of the conditions according to domain order:  </div>
   * <div>1.a. (`b` `≥` domain lower bound) and (`b` `<` domain upper bound).                                </div>
   * <div>1.b. `b` `==` [[ExtendedBound.AboveAll]] - for last bound of iterable.                             </div>
   * 
   * <div>2. Sequence of bounds must be monotonically increasing according to domain order.                  </div>
   * 
   * <div>3. Values associated with adjacent bounds must be different according to `valueOps` order.         </div>
   */
  final class Default[E, D[X] <: Domain[X], V](
    private val iterable: Iterable[BoundValue[E, V]],
    private val domainOps: DomainOps[E, D],
    private val valueOps: ValueOps[V]
  ) extends ValidatingIterable.ValidatingIterableArity1And2[BoundValue[E, V]](
    iterable,
    new DomainBoundsValidation(domainOps),
    new AdjacentBoundsValidation(domainOps).and(new AdjacentValuesValidation(valueOps))
  )

  /**
   * Iterable with single tuple ([[ExtendedBound.AboveAll]], value) to construct ordered map.
   */
  final class Single[E, D[X] <: Domain[X], V](
    private val value: V
  ) extends ValidatingIterable.UncheckedIterable[BoundValue[E, V]](
    List((ExtendedBound.AboveAll, value))
  )

  /**
   * Validation predicate for single tuple (upper bound, value). Returns `true`, iff:
   *  
   * <div>
   *   1. For given bound `b` satisfied at least one of the conditions according to domain order:
   * </div>
   * <div>1.a. (`b` `≥` domain lower bound) and (`b` `<` domain upper bound).  </div>
   * <div>1.b. `b` `==` [[ExtendedBound.AboveAll]].                            </div>
   */
  final class DomainBoundsValidation[E, D[X] <: Domain[X], V](
    private val domainOps: DomainOps[E, D]
  ) extends ValidationPredicate.Arity1[BoundValue[E, V]] {

    override def apply(x: BoundValue[E, V]): Boolean = {
      val ord = domainOps.extendedOrd
      val bound = x._1
      ord.eqv(bound, ExtendedBound.AboveAll) ||
      (ord.lteqv(domainOps.lowerBound, bound) && ord.lt(bound, domainOps.upperBound))
    }

    @throws[ValidationException]("if validation is failed")
    override def validate(x: BoundValue[E, V], index: Long): Unit = 
      if !apply(x) then {
        val bound = x._1
        val showOps = domainOps.showOps
        val boundStr = showOps.extendedShow.show(bound)
        val causeStr = 
          if (domainOps.extendedOrd.eqv(bound, domainOps.upperBound)) {
            "bound must be less than upper bound of domain, " +
            "or use `ExtendedBound.AboveAll` to specify last value of segment sequence"
          } else {
            s"out of domain bounds ${showOps.rangeShow.show(domainOps.boundsRange)}"
          }
        throw ValidationException.invalidBound(boundStr, index, causeStr)
      }
  }

  /**
   * Validation predicate for pair of tuples (`prev` = (upper bound, value), `next` = (upper bound, value)). 
   * Returns `true`, iff:
   *
   * <div>1. (`prev._1` `<` `next._1`) according to domain order.</div>
   */
  final class AdjacentBoundsValidation[E, D[X] <: Domain[X], V](
    private val domainOps: DomainOps[E, D]
  ) extends ValidationPredicate.Arity2[BoundValue[E, V]] {

    override def apply(prev: BoundValue[E, V], next: BoundValue[E, V]): Boolean = 
      domainOps.extendedOrd.lt(prev._1, next._1)

    @throws[ValidationException]("if validation is failed")
    override def validate(prev: BoundValue[E, V], next: BoundValue[E, V], index: Long): Unit = 
      if !apply(prev, next) then {
        val extendedShow = domainOps.showOps.extendedShow
        val prevStr = extendedShow.show(prev._1)
        val nextStr = extendedShow.show(next._1)
        val causeStr = "sequence must be monotonically increasing"
        throw ValidationException.invalidBoundsSeq(prevStr, nextStr, index, causeStr)
      }
  }

  /**
   * Validation predicate for pair of tuples (`prev` = (upper bound, value), `next` = (upper bound, value)). 
   * Returns `true` iff:
   *  
   * <div>1. (`prev._2` `!=` `next._2`) according to `valueEq` typeclass.</div>
   */
  final class AdjacentValuesValidation[E, V](
    private val valueOps: ValueOps[V]
  ) extends ValidationPredicate.Arity2[(ExtendedBound[E], V)] {

    override def apply(prev: (ExtendedBound[E], V), next: (ExtendedBound[E], V)): Boolean = 
      valueOps.neqv(prev._2, next._2)

    @throws[ValidationException]("if validation is failed")
    override def validate(prev: (ExtendedBound[E], V), next: (ExtendedBound[E], V), index: Long): Unit = 
      if !apply(prev, next) then {
        val prevStr = valueOps.show(prev._2)
        val nextStr = valueOps.show(next._2)
        val causeStr = "adjacent values must be non-equal"
        throw ValidationException.invalidValuesSeq(prevStr, nextStr, index, causeStr)
      }
  }
}
