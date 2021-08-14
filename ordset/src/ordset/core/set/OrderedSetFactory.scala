package ordset.core.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.map.{OrderedMap, OrderedMapFactory}
import ordset.core.value.ValueOps
import ordset.core.{Bound, ExtendedBound, SegmentSeqException, SeqValidationPredicate}
import ordset.random.RngManager
import ordset.util.BooleanUtil

import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
 * Unified interface to build ordered sets from collection of upper bounds.
 */
trait OrderedSetFactory[E, D <: Domain[E], +SSeq <: OrderedSet[E, D]] { 
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
   * 1. sequence of bounds MUST be ordered with respect to [[DomainOps.boundOrd]]:
   *
   * bound,,i-1,, `<` bound,,i,, for all i in [1, seq.size]
   *
   * Precondition is controlled by `boundsValidation` function which throws [[SegmentSeqException]] in case of failure.
   * Having `boundsValidation` function as an additional argument allows to omit checks if `bounds` collection
   * is known to be valid, e.g. when it has been supplied by another segment sequence.
   *
   * Implementations are allowed to apply some other validation and throw [[SegmentSeqException]] in case of failure.
   *
   * @param bounds collection of bounds.
   * @param complementary equals to value of the first segment (segment_0).
   *                      If `true` segment is included in set and vise versa.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param boundsValidation function to check precondition 1.
   * @param rngManager generator of random sequences.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuildAsc(
    bounds: IterableOnce[Bound.Upper[E]],
    complementary: Boolean,
    domainOps: DomainOps[E, D]
  )(
    boundsValidation: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation
  )(
    implicit rngManager: RngManager
  ): SSeq

  /**
   * Converts specified `set` into ordered set of type `SSeq`.
   */
  // Note
  // Generic implementation is possible here, but it will be suboptimal. We can't determine the case when conversion
  // isn't required, because we can't pattern match to type `SSeq`. So method is left abstract to be implemented
  // in concrete classes with known type `SSeq`. Generic implementation is provided in method `convertSetInternal`.
  def convertSet(set: OrderedSet[E, D]): SSeq
  
  /**
   * Same as [[unsafeBuildAsc]] but wraps result with [[Try]] catching non-fatal [[Throwable]].
   *
   * Note [[unsafeBuildAsc]] preconditions.
   */
  final def tryBuildAsc(
    bounds: IterableOnce[Bound.Upper[E]],
    complementary: Boolean,
    domainOps: DomainOps[E, D]
  )(
    boundsValidation: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation
  )(
    implicit rngManager: RngManager
  ): Try[SSeq] =
    Try.apply(unsafeBuildAsc(bounds, complementary, domainOps)(boundsValidation)(rngManager))

  /**
   * Get factory with provided parameters (see [[unsafeBuildAsc]] for parameters description).
   */
  final def provided(
    domainOps: DomainOps[E, D],
  )(
    boundsValidation: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation
  )(
    implicit rngManager: RngManager
  ): Partial =
    Partial(domainOps, boundsValidation, rngManager)

  /**
   * Factory with partially provided parameters (see [[unsafeBuildAsc]] for parameters description).
   */
  final case class Partial(
    domainOps: DomainOps[E, D],
    boundsValidation: SeqValidationPredicate[Bound.Upper[E]],
    rngManager: RngManager
  ) {

    /**
     * Same as [[OrderedSetFactory.unsafeBuildAsc]].
     */
    @throws[SegmentSeqException]("if preconditions are violated")
    def unsafeBuildAsc(bounds: IterableOnce[Bound.Upper[E]], complementary: Boolean): SSeq =
      factory.unsafeBuildAsc(bounds, complementary, domainOps)(boundsValidation)(rngManager)

    /**
     * Same as [[OrderedSetFactory.tryBuildAsc]].
     */
    final def tryBuildAsc(bounds: IterableOnce[Bound.Upper[E]], complementary: Boolean): Try[SSeq] =
      Try.apply(unsafeBuildAsc(bounds, complementary))
  }

  // Protected section -------------------------------------------------------- //
  protected final def convertSetInternal(set: OrderedSet[E, D]): SSeq = {
    unsafeBuildAsc(
      set.upperBounds,
      set.firstSegment.value,
      set.domainOps
    )(
      SeqValidationPredicate.alwaysTrue
    )(
      set.rngManager
    )
  }
}

object OrderedSetFactory {

  /**
   * Creates [[OrderedSetFactory]] from specified [[OrderedMapFactory]].
   */
  def fromMapFactory[E, D <: Domain[E], SSeq <: OrderedSet[E, D]](
    mapFactory: OrderedMapFactory[E, D, Boolean, SSeq]
  ): OrderedSetFactory[E, D, SSeq] =
    new MapFactoryWrapper(mapFactory)

  /**
   * Wraps specified [[OrderedMapFactory]] to provide [[OrderedSetFactory]]. 
   * 
   * @note [[convertSet]] may be suboptimal. We can't determine the case when conversion isn't required, 
   *       because we can't pattern match to type `SSeq`. So method should be redefined in classes with
   *       concrete `SSeq` type.
   */
  class MapFactoryWrapper[E, D <: Domain[E], +SSeq <: OrderedSet[E, D]](
    mapFactory: OrderedMapFactory[E, D, Boolean, SSeq]
  ) extends OrderedSetFactory[E, D, SSeq] {

    @throws[SegmentSeqException]("if preconditions are violated")
    def unsafeBuildAsc(
      bounds: IterableOnce[Bound.Upper[E]],
      complementary: Boolean,
      domainOps: DomainOps[E, D]
    )(
      boundsValidation: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation
    )(
      implicit rngManager: RngManager
    ): SSeq = {
      var value = complementary
      val boundsWithValues =
        SeqValidationPredicate.foldIterableAfter[Bound.Upper[E], ListBuffer[(ExtendedBound.Upper[E], Boolean)]](
          bounds,
          boundsValidation,
          ListBuffer.empty,
          (buf, bnd) => {
            buf.addOne((bnd, value))
            value = !value
            buf
          }
        )
      val lastItem = (ExtendedBound.AboveAll, BooleanUtil.inverseN(complementary, boundsWithValues.size))
      boundsWithValues.addOne(lastItem)

      mapFactory.unsafeBuildAsc(
        boundsWithValues,
        domainOps,
        ValueOps.booleanValueOps
      )(
        SeqValidationPredicate.alwaysTrue,  // bounds have been already validated
        SeqValidationPredicate.alwaysTrue   // values always alternate => no validation required
      )(
        rngManager
      )
    }

    override def convertSet(set: OrderedSet[E, D]): SSeq = convertSetInternal(set)
  }
}
