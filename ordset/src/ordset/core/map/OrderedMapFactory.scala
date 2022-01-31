package ordset.core.map

import ordset.core
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.util.SegmentSeqUtil
import ordset.core.value.ValueOps
import ordset.core.{ExtendedBound, Bound, SegmentSeqException, SeqValidationPredicate}
import ordset.random.RngManager

import scala.util.Try

/**
 * Unified interface to build ordered maps from collection of upper bounds and values.
 */
trait OrderedMapFactory[E, D[X] <: Domain[X], V, +SSeq <: OrderedMap[E, D, V]] {
  factory =>

  /**
   * Returns ordered map with specified bounds and values.
   *
   * {{{
   *     seq:
   *
   *         value_0       value_1       value_i        value_(n-1)
   *      ------------) ------------] -------------] -----------X
   *            bound_0       bound_1         bound_i       ExtendedBound.AboveAll
   *
   *                              V V V
   *     ordered map:
   *
   *       segment_0       segment_1     segment_i    segment_(n-1)
   *     X------------)[------------](-------------](-----------X
   *
   *     n = seq.size  - number of segments
   * }}}
   *
   * Preconditions:
   *
   * 1. `seq` must contain at least one item.
   *
   * 2. Last item must contain bound == [[ExtendedBound.AboveAll]] and some value.
   *
   * Preconditions 1 and 2 must be checked by factory implementation. It must throw [[SegmentSeqException]] in case of
   * failure.
   *
   * 3. sequence of bounds must be ordered with respect to [[Domain.extendedOrd]]:
   *
   * bound,,i-1,, `<` bound,,i,, for all i in [1, seq.size - 1]
   *
   * 4. sequence of values must not contain identical adjacent elements:
   *
   * value,,i-1,, != value,,i,, for all i in [1, seq.size - 1]
   *
   * Preconditions 3 and 4 are controlled by `boundsValidation` and `valuesValidation` functions correspondingly
   * which throw [[SegmentSeqException]] in case of failure. Having validation functions as an additional arguments
   * allows to omit checks if `bounds` and `values` collections are known to be valid, e.g. when they have been
   * supplied by another segment sequence.
   *
   * Implementations are allowed to apply some other validation and throw [[SegmentSeqException]] in case of failure.
   *
   * @param seq sequence of (bound, value) tuples
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc.
   * @param boundsValidation function for bounds validation (precondition 3).
   * @param valuesValidation function for values validation (precondition 4).
   * @param rngManager generator of random sequences.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuildAsc(
    seq: IterableOnce[BoundValue[E, V]],
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  )(
    boundsValidation: SeqValidationPredicate[ExtendedBound.Upper[E]] = domainOps.validation.extendedBoundsSeq,
    valuesValidation: SeqValidationPredicate[V] = valueOps.distinctionValidation
  )(
    implicit rngManager: RngManager
  ): SSeq
  
  /**
   * Same as [[unsafeBuildAsc]] but wraps the result with [[Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuildAsc]] preconditions.
   */
  def tryBuildAsc(
    seq: IterableOnce[BoundValue[E, V]],
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  )(
    boundsValidation: SeqValidationPredicate[ExtendedBound.Upper[E]] = domainOps.validation.extendedBoundsSeq,
    valuesValidation: SeqValidationPredicate[V] = valueOps.distinctionValidation
  )(
    implicit rngManager: RngManager
  ): Try[SSeq] =
    Try.apply(unsafeBuildAsc(seq, domainOps, valueOps)(boundsValidation, valuesValidation)(rngManager))

  /**
   * Returns uniform ordered map with specified `value`.
   */
  def buildUniform(
    value: V,
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  )(
    implicit rngManager: RngManager
  ): SSeq =
    unsafeBuildAsc(
      List((ExtendedBound.AboveAll, value)), domainOps, valueOps
    )(
      SeqValidationPredicate.alwaysTrue, SeqValidationPredicate.alwaysTrue
    )(
      rngManager
    )

  /**
   * Returns ordered map with single bound with `value1` below it and `value2` above.
   * {{{
   * 
   *       value1         value2
   *   X------------](-------------X
   *                |
   *              bound
   * }}}
   * 
   * Precondition 4 of [[unsafeBuildAsc]] must be provided. It is controlled by `valuesValidation` function
   * which throws [[SegmentSeqException]] in case of failure
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuildSingleBounded(
    bound: Bound.Upper[E],
    value1: V,
    value2: V,
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  )(
    valuesValidation: SeqValidationPredicate[V] = valueOps.distinctionValidation
  )(
    implicit rngManager: RngManager
  ): SSeq =
    unsafeBuildAsc(
      List((bound, value1), (ExtendedBound.AboveAll, value2)), domainOps, valueOps
    )(
      SeqValidationPredicate.alwaysTrue, valuesValidation
    )(
      rngManager
    )

  /**
   * Same as [[unsafeBuildSingleBounded]] but wraps the result with [[Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuildSingleBounded]] preconditions.
   */
  def tryBuildSingleBounded(
    bound: Bound.Upper[E],
    value1: V,
    value2: V,
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  )(
    valuesValidation: SeqValidationPredicate[V] = valueOps.distinctionValidation
  )(
    implicit rngManager: RngManager
  ): Try[SSeq] =
    Try.apply(unsafeBuildSingleBounded(bound, value1, value2, domainOps, valueOps)(valuesValidation)(rngManager))
    
  /**
   * Converts specified `map` into ordered map of type `SSeq`.
   */
  // Note
  // Generic implementation is possible here, but it will be suboptimal. We can't determine the case when conversion
  // isn't required, because we can't pattern match to type `SSeq`. So method is left abstract to be implemented
  // in concrete classes with known type `SSeq`. Generic implementation is provided in method `convertMapInternal`.
  def convertMap(map: OrderedMap[E, D, V]): SSeq

  /**
   * Get factory with provided parameters (see [[unsafeBuildAsc]] for parameters description).
   */
  final def provided(
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  )(
    boundsValidation: SeqValidationPredicate[ExtendedBound.Upper[E]] = domainOps.validation.extendedBoundsSeq,
    valuesValidation: SeqValidationPredicate[V] = valueOps.distinctionValidation
  )(
    implicit rngManager: RngManager
  ): Partial =
    Partial(domainOps, valueOps, boundsValidation, valuesValidation, rngManager)


  /**
   * Factory with partially provided parameters (see [[unsafeBuildAsc]] for parameters description).
   */
  final case class Partial(
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    boundsValidation: SeqValidationPredicate[ExtendedBound.Upper[E]],
    valuesValidation: SeqValidationPredicate[V],
    rngManager: RngManager
  ) {

    /**
     * Same as [[OrderedMapFactory.unsafeBuildAsc]].
     */
    @throws[SegmentSeqException]("if preconditions are violated")
    final def unsafeBuildAsc(seq: IterableOnce[BoundValue[E, V]]): SSeq =
      factory.unsafeBuildAsc(seq, domainOps, valueOps)(boundsValidation, valuesValidation)(rngManager)

    /**
     * Same as [[OrderedMapFactory.tryBuildAsc]].
     */
    final def tryBuildAsc(seq: IterableOnce[BoundValue[E, V]]): Try[SSeq] = Try.apply(unsafeBuildAsc(seq))

    /**
     * Same as [[OrderedMapFactory.buildUniform]].
     */ 
    final def buildUniform(value: V): SSeq = factory.buildUniform(value, domainOps, valueOps)(rngManager)

    /**
     * Same as [[OrderedMapFactory.unsafeBuildSingleBounded]].
     */ 
    @throws[SegmentSeqException]("if preconditions are violated")
    final def unsafeBuildSingleBounded(bound: Bound.Upper[E], value1: V, value2: V): SSeq = 
      factory.unsafeBuildSingleBounded(bound, value1, value2, domainOps, valueOps)(valuesValidation)(rngManager)

    /**
     * Same as [[OrderedMapFactory.tryBuildSingleBounded]].
     */ 
    def tryBuildSingleBounded(bound: Bound.Upper[E], value1: V, value2: V): Try[SSeq] =
      Try.apply(unsafeBuildSingleBounded(bound, value1, value2))
  }

  // Protected section -------------------------------------------------------- //
  protected final def convertMapInternal(map: OrderedMap[E, D, V]): SSeq =
    unsafeBuildAsc(
      SegmentSeqUtil.getExtendedBoundValueIterableForSeq(map),
      map.domainOps,
      map.valueOps
    )(
      SeqValidationPredicate.alwaysTrue,
      SeqValidationPredicate.alwaysTrue
    )(
      map.rngManager
    )
}
