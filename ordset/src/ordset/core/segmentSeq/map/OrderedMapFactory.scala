package ordset.core.segmentSeq.map

import ordset.core
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.util.SegmentSeqUtil
import ordset.core.value.ValueOps
import ordset.core.{ExtendedBound, Bound}
import ordset.core.segmentSeq.SegmentSeqException
import ordset.core.segmentSeq.validation.ValidatingIterable
import ordset.random.RngManager

import scala.util.Try

/**
 * Unified interface to build ordered maps from collection of upper bounds and values.
 * 
 * @tparam E type of elements on ordered domain
 * @tparam D type of ordered domain
 * @tparam V type of value assigned to interval of elements
 * @tparam SSeq type of output ordered map
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
   * 3. Sequence of bounds must be ordered with respect to [[ordset.core.domain.DomainOps.extendedOrd]]:
   *
   *   bound,,i-1,, `<` bound,,i,, for all i in [1, seq.size - 1]
   * 
   * 4. All bounds in sequence, except last one, must be greater than or equal to the lower bound of
   *    [[ordset.core.domain.DomainOps.domain]]  and less that its upper bound:
   *
   *   domain.lowerBound `≤` bound,,i,, `<` domain.upperBound for all i in [0, seq.size - 2]
   *
   * 5. Sequence of values must not contain identical adjacent elements:
   *
   *   value,,i-1,, != value,,i,, for all i in [1, seq.size - 1]
   *
   * Factory implementations should delegate control of preconditions 3, 4, 5 to input `seq` iterable
   * (see [[ordset.core.segmentSeq.validation.ValidatingIterable]]). This allows client to omit checks, if it's known, 
   * that sequence of bounds and values is valid (e.g. when it was supplied by another segment sequence).
   * 
   * If validation is failed, then [[SegmentSeqException]] is thrown.
   *
   * @param seq collection of tuples (upper bound, value).
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc.
   * @param rngManager generator of random sequences.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuild(
    seq: ValidatingIterable[BoundValue[E, V]]
  )(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): SSeq
  
  /**
   * Same as [[unsafeBuild]] but wraps the result with [[scala.util.Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuild]] preconditions.
   * 
   * @param seq collection of tuples (upper bound, value).
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc.
   * @param rngManager generator of random sequences.
   */
  def tryBuildAsc(
    seq: ValidatingIterable[BoundValue[E, V]]
  )(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): Try[SSeq] =
    Try.apply(unsafeBuild(seq))

  /**
   * Returns uniform ordered map with specified `value`.
   * 
   * @param value value of the single segment.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc.
   * @param rngManager generator of random sequences.
   */
  def buildUniform(
    value: V
  )(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): SSeq =
    unsafeBuild(OrderedMapFactoryIterable.single(value))

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
   * Precondition 4 of [[unsafeBuild]] must be provided. It is controlled by `valuesValidation` function
   * which throws [[SegmentSeqException]] in case of failure
   * 
   * @param bound upper bound of the first segment.
   * @param value1 value of the first segment.
   * @param value2 value of the second segment.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc.
   * @param rngManager generator of random sequences.
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
    unsafeBuild(
      OrderedMapFactoryIterable.default(
        List((bound, value1), (ExtendedBound.AboveAll, value2))
      )
    )

  /**
   * Same as [[unsafeBuildSingleBounded]] but wraps the result with [[scala.util.Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuildSingleBounded]] preconditions.
   * 
   * @param bound upper bound of the first segment.
   * @param value1 value of the first segment.
   * @param value2 value of the second segment.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc.
   * @param rngManager generator of random sequences.
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
   * 
   * @param map ordered map that should be converted.
   */
  // Note
  // Generic implementation is possible here, but it will be suboptimal. We can't determine the case when conversion
  // isn't required, because we can't pattern match to type `SSeq`. So method is left abstract to be implemented
  // in concrete classes with known type `SSeq`. Generic implementation is provided in method `convertMapInternal`.
  def convertMap(map: OrderedMap[E, D, V]): SSeq

  /**
   * Get factory with supplied parameters.
   * 
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc.
   * @param rngManager generator of random sequences.
   */
  final def provided(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): Provided =
    Provided(domainOps, valueOps, rngManager)

  /**
   * Factory with partially supplied parameters.
   */
  final case class Provided(
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ) {

    /**
     * Same as [[OrderedMapFactory.unsafeBuild]].
     */
    @throws[SegmentSeqException]("if preconditions are violated")
    final def unsafeBuild(seq: ValidatingIterable[BoundValue[E, V]]): SSeq =
      factory.unsafeBuild(seq)(domainOps, valueOps, rngManager)

    /**
     * Same as [[OrderedMapFactory.tryBuildAsc]].
     */
    final def tryBuildAsc(seq: ValidatingIterable[BoundValue[E, V]]): Try[SSeq] = Try.apply(unsafeBuild(seq))

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
    unsafeBuild(
      ValidatingIterable.unchecked(
        SegmentSeqUtil.getBoundValueIterableForSeq(map)
      )
    )(
      map.domainOps,
      map.valueOps,
      map.rngManager
    )
}
