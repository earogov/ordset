package ordset.core.map

import ordset.core.domain.Domain
import ordset.core.{Bound, SegmentSeqException}

import scala.util.Try

/**
 * Unified interface to build ordered maps from collection of upper bounds and values.
 */
trait OrderedMapFactory[E, D <: Domain[E], V] {

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
   * If preconditions 1 and 2 are failed [[SegmentSeqException]] MUST be thrown.
   *
   * If preconditions 3 and 4 are failed [[SegmentSeqException]] SHOULD be thrown. Their control may be pushed
   * out of the scope of factory, i.e. we may assume `seq` already provides them.
   *
   * Implementations are allowed to apply some other validation and throw [[SegmentSeqException]] in case of failure.
   *
   * @param seq sequence of (bound, value) tuples
   */
  @throws[SegmentSeqException]("if unable to build valid map with specified bounds and values")
  def unsafeBuildAsc(seq: IterableOnce[(Bound.Upper[E], V)]): OrderedMap[E, D, V]

  /**
   * Same as [[unsafeBuildAsc]] but wraps result with [[Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuildAsc]] preconditions.
   *
   * @param seq sequence of (bound, value) tuples
   */
  def tryBuildAsc(seq: IterableOnce[(Bound.Upper[E], V)]): Try[OrderedMap[E, D, V]] = Try.apply(unsafeBuildAsc(seq))
}
