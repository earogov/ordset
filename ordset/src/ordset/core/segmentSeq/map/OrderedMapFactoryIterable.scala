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
   * <tr>
   *   - all bounds must be between bounds of `domain` OR bound may be [[ExtendedBound.AboveAll]] to specify value
   *     of last segment of ordered map;
   * </tr>
   * <tr>- sequence of bounds must be monotonically increasing according to `domain` order;</tr>
   * <tr>- values associated with adjacent bounds must be different according to `valueOps` order.</tr>
   */
  def default[E, D[X] <: Domain[X], V](
    iterable: IterableOnce[BoundValue[E, V]]
  )(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  ): ValidatingIterable[BoundValue[E, V]] = 
    new DefaultImpl(iterable, domainOps, valueOps)

  /**
   * Returns iterable with single tuple ([[ExtendedBound.AboveAll]], value) to construct ordered map.
   */
  def single[E, D[X] <: Domain[X], V](value: V): ValidatingIterable[BoundValue[E, V]] =
    new SingleImpl(value)

  /**
   * Iterable of tuples (upper bound, value) to construct ordered map.
   * 
   * Iterable provides default validation for ordered maps:
   * <tr>
   *   - all bounds must be between bounds of `domain` OR bound may be [[ExtendedBound.AboveAll]] to specify value
   *     of last segment of ordered map;
   * </tr>
   * <tr>- sequence of bounds must be monotonically increasing according to `domain` order;</tr>
   * <tr>- values associated with adjacent bounds must be different according to `valueOps` order.</tr>
   */
  final class DefaultImpl[E, D[X] <: Domain[X], V](
    private val iterable: IterableOnce[BoundValue[E, V]],
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
  final class SingleImpl[E, D[X] <: Domain[X], V](
    private val value: V
  ) extends ValidatingIterable.UncheckedIterable[BoundValue[E, V]](
    List((ExtendedBound.AboveAll, value))
  )

  /**
   * Validation predicate for single tuple (upper bound, value) such that:
   * <tr>- returns `true` if given bound is between bounds of `domain` OR equals to [[ExtendedBound.AboveAll]].</tr>
   */
  final class DomainBoundsValidation[E, D[X] <: Domain[X], V](
    private val domainOps: DomainOps[E, D]
  ) extends ValidationPredicate.Arity1[BoundValue[E, V]] {

    override def apply(x: BoundValue[E, V]): Boolean = 
      domainOps.extendedOrd.eqv(x._1, ExtendedBound.AboveAll) || domainOps.containsExtended(x._1)

    @throws[ValidationException]("if validation is failed")
    override def validate(x: BoundValue[E, V]): Unit = 
      if !apply(x) then {
        val showOps = domainOps.showOps
        val boundStr = showOps.extendedShow.show(x._1)
        val boundsStr = showOps.rangeShow.show(domainOps.boundsRange)
        val causeStr = s"out of domain bounds $boundsStr"
        throw ValidationException.invalidBound(boundStr, causeStr)
      }
  }

  /**
   * Validation predicate for pair of tuples (upper bound, value) such that:
   * <tr>- returns `true` if first bound is less than second bound according to `domain` order.</tr>
   */
  final class AdjacentBoundsValidation[E, D[X] <: Domain[X], V](
    private val domainOps: DomainOps[E, D]
  ) extends ValidationPredicate.Arity2[BoundValue[E, V]] {

    override def apply(prev: BoundValue[E, V], next: BoundValue[E, V]): Boolean = 
      domainOps.extendedOrd.lt(prev._1, next._1)

    @throws[ValidationException]("if validation is failed")
    override def validate(prev: BoundValue[E, V], next: BoundValue[E, V]): Unit = 
      if !apply(prev, next) then {
        val extendedShow = domainOps.showOps.extendedShow
        val prevStr = extendedShow.show(prev._1)
        val nextStr = extendedShow.show(next._1)
        val causeStr = s"sequence must be monotonically increasing"
        throw ValidationException.invalidBoundsSeq(prevStr, nextStr, causeStr)
      }
  }

  /**
   * Validation predicate for pair of tuples (upper bound, value) such that:
   * <tr>- returns `true` if first value is not equals to second value according to `valueEq` typeclass.</tr>
   */
  final class AdjacentValuesValidation[E, V](
    private val valueOps: ValueOps[V]
  ) extends ValidationPredicate.Arity2[(ExtendedBound[E], V)] {

    override def apply(prev: (ExtendedBound[E], V), next: (ExtendedBound[E], V)): Boolean = 
      valueOps.neqv(prev._2, next._2)

    @throws[ValidationException]("if validation is failed")
    override def validate(prev: (ExtendedBound[E], V), next: (ExtendedBound[E], V)): Unit = 
      if !apply(prev, next) then {
        val prevStr = valueOps.show(prev._2)
        val nextStr = valueOps.show(next._2)
        val causeStr = s"adjacent values must be non-equal"
        throw ValidationException.invalidValuesSeq(prevStr, nextStr, causeStr)
      }
  }
}
