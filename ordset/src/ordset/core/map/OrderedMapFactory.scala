package ordset.core.map

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.{Bound, SegmentSeqException, SeqValidationPredicate}
import ordset.random.RngManager

import scala.util.Try

/**
 * Unified interface to build ordered maps from collection of upper bounds and values.
 */
trait OrderedMapFactory[E, D <: Domain[E], V, +SSeq <: OrderedMap[E, D, V]] {
  factory =>

  /**
   * Returns ordered map with specified bounds and values.
   *
   * {{{
   *     seq:
   *
   *         value_0       value_1       value_i        value_(n-1)
   *      ------------) ------------] -------------] -----------
   *               bound_0       bound_1         bound_i       null
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
   * 1. `seq` must contain at least one element.
   *
   * 2. Last element must contain bound == null and some value (possibly null if it's a meaningful value for given map).
   *
   * Preconditions 1 and 2 must be checked by factory implementation. It must throw [[SegmentSeqException]] in case of
   * failure.
   *
   * 3. sequence of bounds must be ordered with respect to [[Domain.boundOrd]]:
   *
   * bound,,i-1,, `<` bound,,i,, for all i in [1, seq.size - 1]
   *
   * Note: last bound isn't included as it equals to null.
   *
   * 4. sequence of values must not contain identical adjacent elements:
   *
   * value,,i-1,, != value,,i,, for all i in [1, seq.size]
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
  @throws[SegmentSeqException]("if unable to build valid map with specified bounds and values")
  def unsafeBuildAsc(
    seq: IterableOnce[(Bound.Upper[E], V)],
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  )(
    boundsValidation: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation,
    valuesValidation: SeqValidationPredicate[V] = valueOps.distinctionValidation
  )(
    implicit rngManager: RngManager
  ): SSeq

  /**
   * Same as [[unsafeBuildAsc]] but wraps result with [[Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuildAsc]] preconditions.
   */
  def tryBuildAsc(
    seq: IterableOnce[(Bound.Upper[E], V)],
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  )(
    boundsValidation: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation,
    valuesValidation: SeqValidationPredicate[V] = valueOps.distinctionValidation
  )(
    implicit rngManager: RngManager
  ): Try[SSeq] =
    Try.apply(unsafeBuildAsc(seq, domainOps, valueOps)(boundsValidation, valuesValidation)(rngManager))


  /**
   * Get factory with provided parameters (see [[unsafeBuildAsc]] for parameters description).
   */
  final def provided(
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  )(
    boundsValidation: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation,
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
    boundsValidation: SeqValidationPredicate[Bound.Upper[E]],
    valuesValidation: SeqValidationPredicate[V],
    rngManager: RngManager
  ) {

    /**
     * Same as [[OrderedSetFactory.unsafeBuildAsc]].
     */
    @throws[SegmentSeqException]("if preconditions are violated")
    def unsafeBuildAsc(seq: IterableOnce[(Bound.Upper[E], V)]): SSeq =
      factory.unsafeBuildAsc(seq, domainOps, valueOps)(boundsValidation, valuesValidation)(rngManager)

    /**
     * Same as [[OrderedSetFactory.tryBuildAsc]].
     */
    final def tryBuildAsc(seq: IterableOnce[(Bound.Upper[E], V)]): Try[SSeq] = Try.apply(unsafeBuildAsc(seq))
  }
}
