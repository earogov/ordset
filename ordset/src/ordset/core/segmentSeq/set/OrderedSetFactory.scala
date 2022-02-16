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
   * 1. Sequence of bounds must be ordered with respect to [[DomainOps.boundOrd]]:
   *
   *   bound,,i-1,, `<` bound,,i,, for all i in [1, bounds.size - 1]
   * 
   * 2. All bounds in sequence must be greater than or equal to the lower bound of [[DomainOps.domain]] 
   *    and less that its upper bound:
   *
   *   domain.lowerBound `≤` bound,,i,, `<` domain.upperBound for all i in [0, bounds.size - 1]
   *
   * Factory implementations should delegate control of preconditions to input `bounds` iterable
   * (see [[ValidatingIterable]]). This allows client to omit checks, if it's known, that sequence of bounds
   * is valid (e.g. when it was supplied by another segment sequence).
   * 
   * If validation is failed, then [[SegmentSeqException]] is thrown.
   *
   * @param bounds collection of bounds.
   * @param complementary equals to value of the first segment (segment_0).
   *                      If `true` segment is included in set and vise versa.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuildAsc(
    bounds: ValidatingIterable[Bound.Upper[E]],
    complementary: Boolean
  )(
    implicit 
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): SSeq

  /**
   * Same as [[unsafeBuildAsc]] but wraps the result with [[Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuildAsc]] preconditions.
   */
  final def tryBuildAsc(
    bounds: ValidatingIterable[Bound.Upper[E]],
    complementary: Boolean
  )(
    implicit 
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): Try[SSeq] =
    Try.apply(unsafeBuildAsc(bounds, complementary))

  /**
   * Returns uniform ordered set with specified `value`.
   */
  def buildUniform(
    value: Boolean
  )(
    implicit 
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): SSeq =
    unsafeBuildAsc(ValidatingIterable.empty, value)

  /**
   * Returns ordered set with single bound with value equals to `complementary` below it and !`complementary` above.
   * {{{
   *
   *   complementary  !complementary
   *   X------------](-------------X
   *                |
   *              bound
   * }}}
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
    unsafeBuildAsc(OrderedSetFactoryIterable.single(bound)(domainOps), complementary)

  /**
   * Same as [[unsafeBuildSingleBounded]] but wraps the result with [[Try]] catching non-fatal [[Throwable]].
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
   */
  // Note
  // Generic implementation is possible here, but it will be suboptimal. We can't determine the case when conversion
  // isn't required, because we can't pattern match to type `SSeq`. So method is left abstract to be implemented
  // in concrete classes with known type `SSeq`. Generic implementation is provided in method `convertSetInternal`.
  def convertSet(set: OrderedSet[E, D]): SSeq

  /**
   * Get factory with provided parameters (see [[unsafeBuildAsc]] for parameters description).
   */
  final def provided(
    implicit 
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): Partial =
    Partial(domainOps, rngManager)

  /**
   * Factory with partially provided parameters (see [[unsafeBuildAsc]] for parameters description).
   */
  final case class Partial(
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ) {

    /**
     * Same as [[OrderedSetFactory.unsafeBuildAsc]].
     */
    @throws[SegmentSeqException]("if preconditions are violated")
    final def unsafeBuildAsc(bounds: ValidatingIterable[Bound.Upper[E]], complementary: Boolean): SSeq =
      factory.unsafeBuildAsc(bounds, complementary)(domainOps, rngManager)

    /**
     * Same as [[OrderedSetFactory.tryBuildAsc]].
     */
    final def tryBuildAsc(bounds: ValidatingIterable[Bound.Upper[E]], complementary: Boolean): Try[SSeq] =
      Try.apply(unsafeBuildAsc(bounds, complementary))


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
    unsafeBuildAsc(
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
   * Creates [[OrderedSetFactory]] from specified [[OrderedMapFactory]].
   */
  def fromMapFactory[E, D[X] <: Domain[X], SSeq <: OrderedSet[E, D]](
    mapFactory: OrderedMapFactory[E, D, Boolean, SSeq]
  ): OrderedSetFactory[E, D, SSeq] =
    new MapFactoryProxy(mapFactory)

  /**
   * Wraps specified [[OrderedMapFactory]] to provide [[OrderedSetFactory]]. 
   * 
   * @note [[convertSet]] may be suboptimal. We can't determine the case when conversion isn't required, 
   *       because we can't pattern match to type `SSeq`. So method should be redefined in classes with
   *       concrete `SSeq` type.
   */
  class MapFactoryProxy[E, D[X] <: Domain[X], +SSeq <: OrderedSet[E, D]](
    mapFactory: OrderedMapFactory[E, D, Boolean, SSeq]
  ) extends OrderedSetFactory[E, D, SSeq] {

    @throws[SegmentSeqException]("if preconditions are violated")
    def unsafeBuildAsc(
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

        mapFactory.unsafeBuildAsc(
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