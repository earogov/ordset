package ordset.core.segmentSeq.map

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.segmentSeq.SegmentSeqException
import ordset.core.interval.{Interval, IntervalRelation}
import ordset.core.ExtendedBound
import ordset.core.segmentSeq.validation.{ValidationException, ValidatingIterable}
import ordset.random.RngManager
import scala.util.Try
import scala.util.control.NonFatal
import scala.collection.mutable.ListBuffer

/**
 * Unified interface to build ordered maps from collection of interval relations.
 * 
 * @tparam E type of elements on ordered domain
 * @tparam D type of ordered domain
 * @tparam V type of value assigned to range of elements
 * @tparam SSeq type of output ordered set
 */
trait OrderedMapBuilder[E, D[X] <: Domain[X], V, +SSeq <: OrderedMap[E, D, V]] {
  builder =>

  /**
   * Returns ordered map such that:
   * <div>
   *   - elements included in input interval relations are associated with a value of corresponding relation;
   * </div>
   * <div>
   *   - all other elements are associated with specified `defaultValue`.                                      
   * </div>
   * {{{
   * 
   *     input interval relations:
   *
   *                  A                 B
   *     X       (--------]        [---------]         X
   * 
   *     output ordered map:
   *
   *         D        A        D        B          D
   *     X------](--------](------)[---------](--------X
   *    domain                      segment_i        domain
   *    lower bound                                  upper bound
   * 
   *    where D - default value (equals to `defaultValue` argument)
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
   * 3. Input intervals must be ordered by their bounds and must not overlap:
   * 
   *   flip(upperBound,,i-1,,) `<=` lowerBound,,i,, for all i in [1, intervals.size - 1]
   * 
   *   <div>where                                                                                              </div>
   *   <div>lowerBound,,i,, - lower bound of interval i;                                                       </div>
   *   <div>upperBound,,i,, - upper bound of interval i;                                                       </div>
   *   <div>flip(b)         - flip operator (see [[ExtendedBound.flipLimited]]).                               </div>
   * 
   * 4. Value of each interval must not be equal to specified `defaultValue` according to `valueOps` order.
   * 
   * 5. Adjacent intervals must have different values according to `valueOps` order:
   *
   *    if flip(upperBound,,i-1,,) `==` lowerBound,,i,, then value,,i-1,, `!=` value,,i,,
   * 
   *   <div>where                                                                                              </div>
   *   <div>lowerBound,,i,, - lower bound of interval i;                                                       </div>
   *   <div>upperBound,,i,, - upper bound of interval i;                                                       </div>
   *   <div>flip(b)         - flip operator (see [[ExtendedBound.flipLimited]]);                               </div>  
   *   <div>value,,i,,      - value of interval i.                                                             </div>
   * 
   * If validation is failed, then [[SegmentSeqException]] is thrown.
   * 
   * @param defaultValue value assigned to elements that are not included in specified intervals.
   * @param intervals collection of intervals.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc. 
   * @param rngManager generator of random sequences.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuild(
    defaultValue: V,
    intervals: Iterable[IntervalRelation[E, D, V]]
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
   * @param defaultValue value assigned to elements that are not included in specified intervals.
   * @param intervals collection of intervals.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc. 
   * @param rngManager generator of random sequences.
   */
  def tryBuild(
    defaultValue: V,
    intervals: Iterable[IntervalRelation[E, D, V]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): Try[SSeq] = Try.apply { unsafeBuild(defaultValue, intervals) }

  /**
   * Get builder with supplied parameters.
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
   * Builder with supplied parameters.
   */
  final case class Provided(
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ) {

    /**
     * Same as [[OrderedMapBuilder.unsafeBuild]].
     */
    @throws[SegmentSeqException]("if preconditions are violated")
    def unsafeBuild(
      defaultValue: V,
      intervals: Iterable[IntervalRelation[E, D, V]]
    ): SSeq =
      builder.unsafeBuild(defaultValue, intervals)(domainOps, valueOps, rngManager)

    /**
     * Same as [[OrderedMapBuilder.tryBuild]].
     */
    final def tryBuild(
      defaultValue: V,
      intervals: Iterable[IntervalRelation[E, D, V]]
    ): Try[SSeq] = 
      Try.apply { unsafeBuild(defaultValue, intervals) }
  }
}

object OrderedMapBuilder {

  /**
   * Returns ordered map builder based on specified factory (see [[OrderedMapFactory]]).
   * 
   * @param factory ordered map factory.
   */
  def default[E, D[X] <: Domain[X], V, SSeq <: OrderedMap[E, D, V]](
    factory: OrderedMapFactory[E, D, V, SSeq]
  ): DefaultImpl[E, D, V, SSeq] = 
    new DefaultImpl(factory)

  class DefaultImpl[E, D[X] <: Domain[X], V, +SSeq <: OrderedMap[E, D, V]](
    val factory: OrderedMapFactory[E, D, V, SSeq]
  ) extends OrderedMapBuilder[E, D, V, SSeq] {

    @throws[SegmentSeqException]("if preconditions are violated")
    override def unsafeBuild(
      defaultValue: V,
      intervals: Iterable[IntervalRelation[E, D, V]]
    )(
      implicit
      domainOps: DomainOps[E, D],
      valueOps: ValueOps[V],
      rngManager: RngManager
    ): SSeq =
      try {
        val boundValues = 
          mapToBoundValues(defaultValue, OrderedMapBuilderIterable.default(intervals, defaultValue))

        factory.unsafeBuild(boundValues)(domainOps, valueOps, rngManager)

      } catch {
        case NonFatal(e) => throw SegmentSeqException.seqBuildFailed(e)
      }

    @throws[ValidationException]("if validation is failed")
    private def mapToBoundValues(
      defaultValue: V,
      intervals: OrderedMapBuilderIterable.Default[E, D, V]
    )(
      implicit
      domainOps: DomainOps[E, D],
      valueOps: ValueOps[V],
      rngManager: RngManager
    ): ValidatingIterable[BoundValue[E, V]] = {

      // Note, implementation relies on guaranties, that are provided by input `intervals` iterable 
      // (see OrderedMapBuilderIterable.DefaultImpl).

      val ord = domainOps.extendedOrd
      val factory = domainOps.intervals.factory

      val buf = new ListBuffer[BoundValue[E, V]]()
      val iter = intervals.iterator

      var lowerBound: ExtendedBound.Lower[E] = domainOps.lowerBound
      var closed = false
      while (iter.hasNext) {
        val relation = iter.next()
        iter.validate()
        val interval = relation.interval
        interval match {
          case interval: Interval.BoundedBelow[E, D] =>
            val upperBound = interval.lower.flipLower
            if (ord.lteqv(lowerBound, upperBound)) {
              // Gaps between input interval relations are included in map with `defaultValue`.
              //
              //                 previous               current
              //                 interval               interval
              //                 relation               relation
              //       X        (---------)[         ](-----------]
              //    domain                 /         \
              //  lower bound       `lowerBound`     `upperBound`
              buf.addOne((upperBound, defaultValue))
              lowerBound = interval.lower
            }
          case _ => {}
        }
        interval match {
          case interval: Interval.BoundedAbove[E, D] =>
            val upperBound = interval.upper
            if (ord.lteqv(lowerBound, upperBound)) {
              if (ord.lt(upperBound, domainOps.upperBound)) {
                // For each input interval relation add tuple of its upper bound and value.
                //
                //                         current
                //                         interval
                //                         relation
                //       X               (-----------]
                //    domain             /            \
                //  lower bound    `lowerBound`   `upperBound`
                buf.addOne((upperBound, relation.value))
                lowerBound = upperBound.flipUpper
              } else {
                // For last interval relation add tuple with greatest possible bound and value of relation.
                //
                //            current
                //            interval   `upperBound`
                //            relation  /
                //          (----------]
                //         /           X
                //  `lowerBound`    domain
                //                upper bound
                buf.addOne((ExtendedBound.AboveAll, relation.value))
                closed = true
              }
            }
          case _ =>
            // For last interval relation add tuple with greatest possible bound and value of relation.
            buf.addOne((ExtendedBound.AboveAll, relation.value))
            closed = true
        }
      }
      // If upper bound of last interval relation was less than upper bound of domain, then add tuple of
      // greatest possible bound and default value.
      //
      //            last
      //            interval
      //            relation
      //          (----------]
      //                                 X
      //                               domain
      //                             upper bound
      if (!closed) buf.addOne((ExtendedBound.AboveAll, defaultValue))
      // There is no need to validate output iterable of bounds and values. Current implementation guarantees, that:
      // - All bounds except last one are greater than or equal to lower bound of domain and less than its upper bound.
      // - Last bound equals to [[ExtendedBound.AboveAll]].
      // - Sequence of bounds is monotonically increasing according to domain order.
      // - Values associated with adjacent bounds are different  according to `valueOps` order.
      ValidatingIterable.unchecked(buf.toList)
    }
  }
}
