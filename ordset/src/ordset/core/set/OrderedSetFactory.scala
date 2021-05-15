package ordset.core.set

import ordset.core.domain.Domain
import ordset.core.{Bound, SegmentSeqException}

import scala.util.Try

/**
 * Unified interface to build ordered sets from collection of upper bounds.
 */
trait OrderedSetFactory[E, D <: Domain[E]] {

  /**
   * Returns ordered set with specified bounds.
   *
   * {{{
   *     bounds:
   *
   *      ------------) ------------] -------------]
   *               bound_0       bound_i        bound_(n-2)
   *
   *                              V V V
   *     ordered set:
   *
   *       segment_0      segment_i    segment_(n-2)   segment_(n-1)
   *     X------------)[------------](-------------](-----------X
   *
   *
   *     n = (bounds.size + 1)  - number of segments
   *     segment_0.value == complementary, segment_1.value == !complementary, ...
   * }}}
   *
   * Preconditions:
   *
   * 1. sequence of bounds must be ordered with respect to [[Domain.boundOrd]]:
   *
   * bound,,i-1,, `<` bound,,i,, for all i in [1, seq.size]
   *
   * If precondition is failed [[SegmentSeqException]] SHOULD be thrown. Its control may be pushed
   * out of the scope of factory, i.e. we may assume `seq` already provides it.
   *
   * Implementations are allowed to apply some other validation and throw [[SegmentSeqException]] in case of failure.
   *
   * @param bounds collection of bounds.
   * @param complementary equals to value of the first segment (segment_0).
   *                      If `true` segment is included in set and vise versa.
   */
  @throws[SegmentSeqException]("if unable to build valid set with specified bounds")
  def unsafeBuildAsc(bounds: IterableOnce[Bound.Upper[E]], complementary: Boolean): OrderedSet[E, D]

  /**
   * Same as [[unsafeBuildAsc]] but wraps result with [[Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuildAsc]] preconditions.
   *
   * @param bounds collection of bounds.
   * @param complementary equals to value of the first segment (segment_0).
   *                      If `true` segment is included in set and vise versa.
   */
  def tryBuildAsc(bounds: IterableOnce[Bound.Upper[E]], complementary: Boolean): Try[OrderedSet[E, D]] =
    Try.apply(unsafeBuildAsc(bounds, complementary))
}
