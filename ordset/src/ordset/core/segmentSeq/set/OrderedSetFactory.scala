package ordset.core.segmentSeq.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.{Bound, ExtendedBound}
import ordset.core.segmentSeq.SegmentSeqException
import ordset.core.segmentSeq.map.{OrderedMap, OrderedMapFactory, BoundValue}
import ordset.core.segmentSeq.validation.{ValidatingIterable, ValidationException}
import ordset.random.RngManager
import ordset.util.BooleanUtil

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.control.NonFatal

/**
 * Unified interface to build ordered sets from collection of upper bounds.
 * 
 * @tparam E type of elements on ordered domain
 * @tparam D type of ordered domain
 * @tparam SSeq type of output ordered set
 */
trait OrderedSetFactory[E, D[X] <: Domain[X], +SSeq <: OrderedSet[E, D]] { 
  factory =>

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
   * 1. Sequence of bounds must be ordered with respect to [[ordset.core.domain.DomainOps.boundOrd]]:
   *
   *   bound,,i-1,, `<` bound,,i,, for all i in [1, bounds.size - 1]
   * 
   * 2. All bounds in sequence must be greater than or equal to the lower bound of 
   *    [[ordset.core.domain.DomainOps.domain]] and less that its upper bound:
   *
   *   domain.lowerBound `≤` bound,,i,, `<` domain.upperBound for all i in [0, bounds.size - 1]
   *
   * Factory implementations should delegate control of preconditions to input `bounds` iterable
   * (see [[ordset.core.segmentSeq.validation.ValidatingIterable]]). This allows client to omit checks, if it's known, 
   * that sequence of bounds is valid (e.g. when it was supplied by another segment sequence).
   * 
   * If validation is failed, then [[SegmentSeqException]] is thrown.
   *
   * @param bounds collection of bounds.
   * @param complementary equals to value of the first segment (segment_0).
   *                      If `true`, segment is included in set and vise versa.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuild(
    bounds: ValidatingIterable[Bound.Upper[E]],
    complementary: Boolean
  )(
    implicit 
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): SSeq

  /**
   * Same as [[unsafeBuild]] but wraps the result with [[scala.util.Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuild]] preconditions.
   * 
   * @param bounds collection of bounds.
   * @param complementary equals to value of the first segment; if `true`, segment is included in set and vise versa.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  final def tryBuildAsc(
    bounds: ValidatingIterable[Bound.Upper[E]],
    complementary: Boolean
  )(
    implicit 
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): Try[SSeq] =
    Try.apply(unsafeBuild(bounds, complementary))

  /**
   * Returns uniform ordered set with specified `value`.
   * 
   * @param value value of the single segment.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  def buildUniform(
    value: Boolean
  )(
    implicit 
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): SSeq =
    unsafeBuild(ValidatingIterable.empty, value)

  /**
   * Returns ordered set with single bound with value equals to `complementary` below it and !`complementary` above.
   * {{{
   *
   *   complementary  !complementary
   *   X------------](-------------X
   *                |
   *              bound
   * }}}
   * 
   * @param bound upper bound of the first segment.
   * @param complementary equals to value of the first segment; if `true`, segment is included in set and vise versa.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuildSingleBounded(
    bound: Bound.Upper[E],
    complementary: Boolean
  )(
    implicit 
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): SSeq =
    unsafeBuild(OrderedSetFactoryIterable.single(bound)(domainOps), complementary)

  /**
   * Same as [[unsafeBuildSingleBounded]] but wraps the result with [[scala.util.Try]] catching non-fatal [[Throwable]].
   * 
   * @param bound upper bound of the first segment.
   * @param complementary equals to value of the first segment; if `true`, segment is included in set and vise versa.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  def tryBuildSingleBounded(
    bound: Bound.Upper[E],
    complementary: Boolean
  )(
    implicit 
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): Try[SSeq] =
    Try.apply(unsafeBuildSingleBounded(bound, complementary))

  /**
   * Converts specified `set` into ordered set of type `SSeq`.
   * 
   * @param set ordered set that should be converted.
   */
  // Note
  // Generic implementation is possible here, but it will be suboptimal. We can't determine the case when conversion
  // isn't required, because we can't pattern match to type `SSeq`. So method is left abstract to be implemented
  // in concrete classes with known type `SSeq`. Generic implementation is provided in method `convertSetInternal`.
  def convertSet(set: OrderedSet[E, D]): SSeq

  /**
   * Get factory with supplied parameters.
   * 
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  final def provided(
    implicit 
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): Provided =
    Provided(domainOps, rngManager)

  /**
   * Factory with supplied parameters.
   */
  final case class Provided(
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ) {

    /**
     * Same as [[OrderedSetFactory.unsafeBuild]].
     */
    @throws[SegmentSeqException]("if preconditions are violated")
    final def unsafeBuild(bounds: ValidatingIterable[Bound.Upper[E]], complementary: Boolean): SSeq =
      factory.unsafeBuild(bounds, complementary)(domainOps, rngManager)

    /**
     * Same as [[OrderedSetFactory.tryBuildAsc]].
     */
    final def tryBuildAsc(bounds: ValidatingIterable[Bound.Upper[E]], complementary: Boolean): Try[SSeq] =
      Try.apply(unsafeBuild(bounds, complementary))


    /**
     * Same as [[OrderedSetFactory.buildUniform]].
     */ 
    final def buildUniform(value: Boolean): SSeq = factory.buildUniform(value)(domainOps, rngManager)

    /**
     * Same as [[OrderedSetFactory.unsafeBuildSingleBounded]].
     */ 
    @throws[SegmentSeqException]("if preconditions are violated")
    final def unsafeBuildSingleBounded(bound: Bound.Upper[E], complementary: Boolean): SSeq = 
      factory.unsafeBuildSingleBounded(bound, complementary)(domainOps, rngManager)

    /**
     * Same as [[OrderedSetFactory.tryBuildSingleBounded]].
     */ 
    def tryBuildSingleBounded(bound: Bound.Upper[E], complementary: Boolean): Try[SSeq] =
      Try.apply(unsafeBuildSingleBounded(bound, complementary))
  }

  // Protected section -------------------------------------------------------- //
  protected final def convertSetInternal(set: OrderedSet[E, D]): SSeq = {
    unsafeBuild(
      ValidatingIterable.unchecked(set.upperBounds),
      set.firstSegment.value
    )(
      set.domainOps,
      set.rngManager
    )
  }
}

object OrderedSetFactory {

  /**
   * Creates [[OrderedSetFactory]] from specified [[ordset.core.segmentSeq.map.OrderedMapFactory]].
   */
  def fromMapFactory[E, D[X] <: Domain[X], SSeq <: OrderedSet[E, D]](
    mapFactory: OrderedMapFactory[E, D, Boolean, SSeq]
  ): OrderedSetFactory[E, D, SSeq] =
    new MapFactoryProxy(mapFactory)

  /**
   * Wraps specified [[ordset.core.segmentSeq.map.OrderedMapFactory]] to provide [[OrderedSetFactory]]. 
   * 
   * @note [[convertSet]] may be suboptimal. We can't determine the case when conversion isn't required, 
   *       because we can't pattern match to type `SSeq`. So method should be redefined in classes with
   *       concrete `SSeq` type.
   */
  class MapFactoryProxy[E, D[X] <: Domain[X], +SSeq <: OrderedSet[E, D]](
    mapFactory: OrderedMapFactory[E, D, Boolean, SSeq]
  ) extends OrderedSetFactory[E, D, SSeq] {

    @throws[SegmentSeqException]("if preconditions are violated")
    def unsafeBuild(
      bounds: ValidatingIterable[Bound.Upper[E]],
      complementary: Boolean
    )(
      implicit 
      domainOps: DomainOps[E, D],
      rngManager: RngManager
    ): SSeq = 
      try {
        var value = complementary
        
        val boundsValues = 
          OrderedSetFactoryIterable.default(bounds)
            .foldLeftValidated(ListBuffer.empty[BoundValue[E, Boolean]]) { (buf, bnd) =>
              buf.addOne((bnd, value))
              value = !value
              buf
            }

        val lastItem = (ExtendedBound.AboveAll, BooleanUtil.inverseN(complementary, boundsValues.size))
        boundsValues.addOne(lastItem)

        mapFactory.unsafeBuild(
          // Bounds have been already validated.
          // Values always alternate => no validation required.
          ValidatingIterable.unchecked(boundsValues)
        )(
          domainOps,
          ValueOps.booleanValueOps,
          rngManager
        )
    } catch {
      case NonFatal(e) => throw SegmentSeqException.seqBuildFailed(e)
    }

    override def convertSet(set: OrderedSet[E, D]): SSeq = convertSetInternal(set)
  }
}