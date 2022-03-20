package ordset.core.segmentSeq.set

import ordset.core.{Bound, ExtendedBound}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.interval.Interval
import ordset.util.NullableUtil
import ordset.random.RngManager
import scala.util.Try
import ordset.core.segmentSeq.SegmentSeqException
import scala.collection.mutable.ListBuffer
import ordset.core.segmentSeq.validation.{ValidatingIterable, ValidationException}
import scala.util.control.NonFatal

/**
 * Unified interface to build ordered sets from collection of intervals.
 * 
 * @tparam E type of elements on ordered domain
 * @tparam D type of ordered domain
 * @tparam SSeq type of output ordered set
 */
trait OrderedSetBuilder[E, D[X] <: Domain[X], +SSeq <: OrderedSet[E, D]] {
  builder =>
  
  /**
   * Returns ordered set, that includes all elements in specified intervals.
   * {{{
   * 
   *     input intervals:
   *
   *     X         [--------]           [---------X
   * 
   *     output ordered set:
   * 
   *        false     true      false      true
   *     X--------)[--------](---------)[---------X
   *    domain                segment_i        domain
   *    lower bound                            upper bound
   * }}}
   * 
   * Preconditions:
   *
   * 1. All intervals must be non-empty.  
   *
   * 2. Lower and upper bounds of each interval must be between domain bounds:
   * 
   *   domain.lower `≤` lowerBound,,i,, `≤` domain.upper for all i in [0, intervals.size - 1]
   * 
   *   domain.lower `≤` upperBound,,i,, `≤` domain.upper for all i in [0, intervals.size - 1]
   * 
   *   <div>where                                                                                              </div>
   *   <div>lowerBound,,i,, - lower bound of interval i;                                                       </div>
   *   <div>upperBound,,i,, - upper bound of interval i;                                                       </div>
   *   <div>domain.lower    - lower bound of domain;                                                           </div>
   *   <div>domain.upper    - upper bound of domain.                                                           </div>
   *
   * 3. Input intervals must be ordered by their bounds, they must not overlap and there must be a gap between
   *    each pair:
   * 
   *   flip(upperBound,,i-1,,) `<` lowerBound,,i,, for all i in [1, intervals.size - 1]
   * 
   *   <div>where                                                                                              </div>
   *   <div>lowerBound,,i,, - lower bound of interval i;                                                       </div>
   *   <div>upperBound,,i,, - upper bound of interval i;                                                       </div>
   *   <div>flip(b)         - flip operator (see [[ExtendedBound.flipLimited]]).                               </div>
   * 
   * If validation is failed, then [[SegmentSeqException]] is thrown.
   * 
   * @param intervals collection of intervals.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuild(
    intervals: Iterable[Interval[E, D]]
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
   * @param intervals collection of intervals.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  def tryBuild(
    intervals: Iterable[Interval[E, D]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): Try[SSeq] = Try.apply { unsafeBuild(intervals) }

  /**
   * Get builder with supplied parameters.
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
   * Builder with supplied parameters.
   */
  final case class Provided(
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ) {

    /**
     * Same as [[OrderedSetBuilder.unsafeBuild]].
     */
    @throws[SegmentSeqException]("if preconditions are violated")
    def unsafeBuild(intervals: Iterable[Interval[E, D]]): SSeq =
      builder.unsafeBuild(intervals)(domainOps, rngManager)

    /**
     * Same as [[OrderedSetBuilder.tryBuild]].
     */
    final def tryBuild(intervals: Iterable[Interval[E, D]]): Try[SSeq] = 
      Try.apply { unsafeBuild(intervals) }
  }
}

object OrderedSetBuilder {

  /**
   * Returns ordered set builder based on specified factory (see [[OrderedSetFactory]]).
   * 
   * @param factory ordered set factory.
   */
  def default[E, D[X] <: Domain[X], SSeq <: OrderedSet[E, D]](
    factory: OrderedSetFactory[E, D, SSeq]
  ): DefaultImpl[E, D, SSeq] = 
    new DefaultImpl(factory)

  class DefaultImpl[E, D[X] <: Domain[X], +SSeq <: OrderedSet[E, D]](
    val factory: OrderedSetFactory[E, D, SSeq]
  ) extends OrderedSetBuilder[E, D, SSeq] {

    final val valueOps: ValueOps[Boolean] = ValueOps.booleanValueOps

    @throws[SegmentSeqException]("if preconditions are violated")
    override def unsafeBuild(
      intervals: Iterable[Interval[E, D]]
    )(
      implicit
      domainOps: DomainOps[E, D],
      rngManager: RngManager
    ): SSeq =
      try {
        val (complementary, bounds) = 
          mapToBounds(OrderedSetBuilderIterable.default(intervals))

        factory.unsafeBuild(bounds, complementary)(domainOps, rngManager)
        
      } catch {
        case NonFatal(e) => throw SegmentSeqException.seqBuildFailed(e)
      }

    @throws[ValidationException]("if validation is failed")
    private def mapToBounds(
      intervals: OrderedSetBuilderIterable.Default[E, D]
    )(
      implicit
      domainOps: DomainOps[E, D],
      rngManager: RngManager
    ): (Boolean, ValidatingIterable[Bound.Upper[E]]) = {

      // Note, implementation relies on guaranties, that are provided by input `intervals` iterable 
      // (see OrderedSetBuilderIterable.DefaultImpl).

      val ord = domainOps.extendedOrd
      val factory = domainOps.intervals.factory

      val buf = new ListBuffer[Bound.Upper[E]]()
      val iter = intervals.iterator

      var lowerBound: ExtendedBound.Lower[E] = domainOps.lowerBound
      var complementary: Boolean | Null = null
      while (iter.hasNext) {
        val interval = iter.next()
        iter.validate()
        interval match {
          case interval: Interval.BoundedBelow[E, D] =>
            val upperBound = interval.lower.flipLower
            if (ord.lt(lowerBound, upperBound)) {
              // Add upper bound of segment, that represents gap between intervals included in set.
              //
              //                 previous               current
              //                 interval               interval
              //       X        (---------)[         ](-----------]
              //    domain                 /         \
              //  lower bound       `lowerBound`     `upperBound`
              buf.addOne(upperBound)
              lowerBound = interval.lower
              if (complementary == null) complementary = valueOps.unit
            }
          case _ => {}
        }
        interval match {
          case interval: Interval.BoundedAbove[E, D] =>
            val upperBound = interval.upper
            if (ord.lt(lowerBound, upperBound) && ord.lt(upperBound, domainOps.upperBound)) {
              // Add upper bound of segment, that represents interval included in set.
              //
              //                          current
              //                          interval
              //       X               (-----------]
              //    domain             /            \
              //  lower bound    `lowerBound`   `upperBound`
              //
              // If upper bound of interval equals to upper bound of domain, then skip it and stop iterating.
              buf.addOne(upperBound)
              lowerBound = upperBound.flipUpper
            }
          case _ => {}
        }
        if (complementary == null) complementary = !valueOps.unit
      }
      // There is no need to validate output iterable of bounds. Current implementation guarantees, that:
      // - All bounds are greater than or equal to lower bound of domain and less than its upper bound.
      // - Sequence of bounds is monotonically increasing according to domain order.
      (NullableUtil.nnOrElse(complementary, false), ValidatingIterable.unchecked(buf.toList))
    }
  }
}
