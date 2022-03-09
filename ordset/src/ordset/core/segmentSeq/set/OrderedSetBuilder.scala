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
   * 
   * {{{
   *     input intervals:
   *
   *               [--------]           [---------X
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
   * 1. All intervals must have domain equal to `domainOps.domain`.
   *
   * 2. Input intervals must be non-overlapping and ordered by their bounds:
   * 
   *   upper_bound,,i-1,, `<` lower_bound,,i,, for all i in [1, seq.size - 1]
   * 
   *   <div>where                                        </div>
   *   <div>lower_bound,,i,, - lower bound of interval i;</div>
   *   <div>lower_bound,,i,, - upper bound of interval i.</div>
   * 
   * Note, that precondition 1 guaranties, that all elements of intervals belongs to `domainOps.domain`.
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
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
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
    def unsafeBuild(
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
      intervals: OrderedSetBuilderIterable.DefaultImpl[E, D]
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
      var run = true
      while (run && iter.hasNext) {
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
            run = ord.lt(upperBound, domainOps.upperBound)
            if (ord.lt(lowerBound, upperBound)) {
              // Add upper bound of segment, that represents interval included in set.
              //
              //                          current
              //                          interval
              //       X               (-----------]
              //    domain             /            \
              //  lower bound    `lowerBound`   `upperBound`
              //
              // If upper bound of interval equals to upper bound of domain, then skip it and stop iterating.
              if (run) {
                buf.addOne(upperBound)
                lowerBound = interval.upper.flipUpper
              }
              if (complementary == null) complementary = !valueOps.unit
            }
          case _ =>
            if (complementary == null) complementary = !valueOps.unit
            run = false
        }
      }
      // There is no need to validate output iterable of bounds. Current implementation guarantees, that:
      // - All bounds are greater than or equal to lower bound of domain and less than its upper bound.
      // - Sequence of bounds is monotonically increasing according to domain order.
      (NullableUtil.nnOrElse(complementary, false), ValidatingIterable.unchecked(buf.toList))
    }
  }
}
