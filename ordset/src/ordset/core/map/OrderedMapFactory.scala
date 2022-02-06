package ordset.core.map

import ordset.core
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.util.SegmentSeqUtil
import ordset.core.value.ValueOps
import ordset.core.{ExtendedBound, Bound, SegmentSeqException}
import ordset.core.validation.ValidatingIterable
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
   * 2. Last item must have bound, that equals to [[ExtendedBound.AboveAll]].
   *
   * Preconditions 1 and 2 must be checked by factory implementation. It must throw [[SegmentSeqException]] in case of
   * failure.
   *
   * 3. Sequence of bounds must be ordered with respect to [[Domain.extendedOrd]]:
   *
   *   bound,,i-1,, `<` bound,,i,, for all i in [1, seq.size - 1]
   * 
   * 4. All bounds, except last one, must be between bounds of [[DomainOps.domain]]:
   *
   *   domain.lowerBound `≤` bound,,i,, `≤` domain.upperBound for all i in [0, seq.size - 2]
   *
   * 5. Sequence of values must not contain identical adjacent elements:
   *
   *   value,,i-1,, != value,,i,, for all i in [1, seq.size - 1]
   *
   * Factory implementations should delegate control of preconditions 3, 4, 5 to input `seq` iterable
   * (see [[ValidatingIterable]]). This allows client to omit checks, if it's known, that sequence of bounds
   * and values is valid (e.g. when it was supplied by another segment sequence).
   * 
   * If validation is failed, then [[SegmentSeqException]] is thrown.
   *
   * @param seq collection of tuples (upper bound, value).
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc.
   * @param rngManager generator of random sequences.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuildAsc(
    seq: ValidatingIterable[BoundValue[E, V]]
  )(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): SSeq
  
  /**
   * Same as [[unsafeBuildAsc]] but wraps the result with [[Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuildAsc]] preconditions.
   */
  def tryBuildAsc(
    seq: ValidatingIterable[BoundValue[E, V]]
  )(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): Try[SSeq] =
    Try.apply(unsafeBuildAsc(seq))

  /**
   * Returns uniform ordered map with specified `value`.
   */
  def buildUniform(
    value: V
  )(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): SSeq =
    unsafeBuildAsc(OrderedMapFactoryIterable.single(value))

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
    value2: V
  )(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): SSeq =
    unsafeBuildAsc(
      OrderedMapFactoryIterable.default(
        List((bound, value1), (ExtendedBound.AboveAll, value2))
      )
    )

  /**
   * Same as [[unsafeBuildSingleBounded]] but wraps the result with [[Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuildSingleBounded]] preconditions.
   */
  def tryBuildSingleBounded(
    bound: Bound.Upper[E],
    value1: V,
    value2: V
  )(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): Try[SSeq] =
    Try.apply(unsafeBuildSingleBounded(bound, value1, value2))
    
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
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): Partial =
    Partial(domainOps, valueOps, rngManager)


  /**
   * Factory with partially provided parameters (see [[unsafeBuildAsc]] for parameters description).
   */
  final case class Partial(
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ) {

    /**
     * Same as [[OrderedMapFactory.unsafeBuildAsc]].
     */
    @throws[SegmentSeqException]("if preconditions are violated")
    final def unsafeBuildAsc(seq: ValidatingIterable[BoundValue[E, V]]): SSeq =
      factory.unsafeBuildAsc(seq)(domainOps, valueOps, rngManager)

    /**
     * Same as [[OrderedMapFactory.tryBuildAsc]].
     */
    final def tryBuildAsc(seq: ValidatingIterable[BoundValue[E, V]]): Try[SSeq] = Try.apply(unsafeBuildAsc(seq))

    /**
     * Same as [[OrderedMapFactory.buildUniform]].
     */ 
    final def buildUniform(value: V): SSeq = factory.buildUniform(value)(domainOps, valueOps, rngManager)

    /**
     * Same as [[OrderedMapFactory.unsafeBuildSingleBounded]].
     */ 
    @throws[SegmentSeqException]("if preconditions are violated")
    final def unsafeBuildSingleBounded(bound: Bound.Upper[E], value1: V, value2: V): SSeq = 
      factory.unsafeBuildSingleBounded(bound, value1, value2)(domainOps, valueOps, rngManager)

    /**
     * Same as [[OrderedMapFactory.tryBuildSingleBounded]].
     */ 
    def tryBuildSingleBounded(bound: Bound.Upper[E], value1: V, value2: V): Try[SSeq] =
      Try.apply(unsafeBuildSingleBounded(bound, value1, value2))
  }

  // Protected section -------------------------------------------------------- //
  protected final def convertMapInternal(map: OrderedMap[E, D, V]): SSeq =
    unsafeBuildAsc(
      ValidatingIterable.unchecked(
        SegmentSeqUtil.getBoundValueIterableForSeq(map)
      )
    )(
      map.domainOps,
      map.valueOps,
      map.rngManager
    )
}
