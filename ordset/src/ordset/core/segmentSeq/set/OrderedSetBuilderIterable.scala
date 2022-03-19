package ordset.core.segmentSeq.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.validation.{ValidationPredicate, ValidationException, ValidatingIterable}
import ordset.core.interval.Interval

object OrderedSetBuilderIterable {

  /**
   * Returns iterable of intervals to construct ordered set.
   * 
   * Iterable provides default validation for ordered sets:
   *  
   * <div>1. All intervals must be non-empty.                                                                  </div>
   * 
   * <div>2. Lower and upper bounds of each interval `i` must be between domain bounds                         </div>
   * <div>   according to domain order:                                                                        </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.lower` `≤` `domainOps.upperBound`)                                 </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.upper` `≤` `domainOps.upperBound`)                                 </div>
   * 
   * <div>3. For each pair of intervals `prev` and `next` must be satisfied condition:                         </div>
   * <div>   (`prev.upper.flipLimited` `<` `next.lower`) according to domain order                             </div>
   * <div>   (see [[ExtendedBound.flipLimited]]).                                                              </div>
   * <div>   I.e. `next` interval must follow after `prev`, and there must be a gap between them.              </div>
   * <div>   Intervals that follows without gap can be always merged into one.                                 </div>
   */
  def default[E, D[X] <: Domain[X]](
    iterable: Iterable[Interval[E, D]]
  )(
    implicit domainOps: DomainOps[E, D]
  ): Default[E, D] =
    new Default(iterable, domainOps)

  /**
   * Iterable of intervals to construct ordered set.
   * 
   * Iterable provides default validation for ordered sets:
   * 
   * <div>1. All intervals must be non-empty.                                                                  </div>
   * 
   * <div>2. Lower and upper bounds of each interval `i` must be between domain bounds                         </div>
   * <div>   according to domain order:                                                                        </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.lower` `≤` `domainOps.upperBound`)                                 </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.upper` `≤` `domainOps.upperBound`)                                 </div>
   * 
   * <div>3. For each pair of intervals `prev` and `next` must be satisfied condition:                         </div>
   * <div>   (`prev.upper.flipLimited` `<` `next.lower`) according to domain order                             </div>
   * <div>   (see [[ExtendedBound.flipLimited]]).                                                              </div>
   * <div>   I.e. `next` interval must follow after `prev`, and there must be a gap between them.              </div>
   * <div>   Intervals that follows without gap can be always merged into one.                                 </div>
   */
  final class Default[E, D[X] <: Domain[X]](
    private val iterable: Iterable[Interval[E, D]],
    private val domainOps: DomainOps[E, D]
  ) extends ValidatingIterable.ValidatingIterableArity1And2[Interval[E, D]](
    iterable,
    new IntervalBoundsValidation(domainOps),
    new AdjacentIntervalsValidation(domainOps)
  )

  /**
   * Validation predicate for single interval. Returns `true`, iff:
   *  
   * <div>1. Interval is non-empty.                                                                            </div>
   * 
   * <div>2. Lower and upper bounds of interval `i` are between domain bounds according to domain order:       </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.lower` `≤` `domainOps.upperBound`)                                 </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.upper` `≤` `domainOps.upperBound`)                                 </div>
   */
  final class IntervalBoundsValidation[E, D[X] <: Domain[X]](
    private val domainOps: DomainOps[E, D]
  ) extends ValidationPredicate.Arity1[Interval[E, D]] {

    override def apply(x: Interval[E, D]): Boolean = 
      x match {
        case x: Interval.NonEmpty[E, D] => domainOps.containsExtended(x.lower) && domainOps.containsExtended(x.upper)
        case _ => false
      }

    @throws[ValidationException]("if validation is failed")
    override def validate(x: Interval[E, D], index: Long): Unit = 
      x match {
        case x: Interval.NonEmpty[E, D] =>
          val invalidBound: String | Null = 
            if (!domainOps.containsExtended(x.lower)) "lower"
            else if (!domainOps.containsExtended(x.upper)) "upper"
            else null

          if (invalidBound != null) {
            val showOps = domainOps.showOps
            val intervalStr = showOps.intervalShow.show(x)
            val boundsStr = showOps.rangeShow.show(domainOps.boundsRange)
            val causeStr = s"$invalidBound bound of interval is out of domain bounds $boundsStr"
            throw ValidationException.invalidInterval(intervalStr, index, causeStr)
          }
        case _ =>
          val show = domainOps.showOps.intervalShow
          val causeStr = "interval must be non-empty"
          val intervalStr = show.show(x)
          throw ValidationException.invalidInterval(intervalStr, index, causeStr)
      }
  }

  /**
   * Validation predicate for pair of intervals (`prev`, `next`). Returns `true`, iff:
   *  
   * <div>1. Both `prev` and `next` are non-empty intervals.                                                    </div>
   * 
   * <div>2. (`prev.upper.flipLimited` `<` `next.lower`) according to domain order                              </div>
   * <div>   (see [[ExtendedBound.flipLimited]]).                                                               </div>
   * <div>   I.e. `next` interval must follow after `prev`, and there must be a gap between them.               </div>
   * <div>   Intervals that follow without gap should be merged into one.                                       </div>
   */
  final class AdjacentIntervalsValidation[E, D[X] <: Domain[X]](
    private val domainOps: DomainOps[E, D]
  ) extends ValidationPredicate.Arity2[Interval[E, D]] {

    override def apply(prev: Interval[E, D], next: Interval[E, D]): Boolean = 
      (prev, next) match {
        case (prev: Interval.NonEmpty[E, D], next: Interval.NonEmpty[E, D]) => prev.isSeparatedPrecedingNE(next)
        case _ => false
      }

    @throws[ValidationException]("if validation is failed")
    override def validate(prev: Interval[E, D], next: Interval[E, D], index: Long): Unit =
      (prev, next) match {
        case (prev: Interval.NonEmpty[E, D], next: Interval.NonEmpty[E, D]) =>
          if (!prev.isSeparatedPrecedingNE(next)) {
            val ord = domainOps.extendedOrd
            val show = domainOps.showOps.intervalShow
            val prevStr = show.show(prev)
            val nextStr = show.show(next)
            val causeStr = 
              if (ord.lteqv(next.lower, prev.lower)) "intervals must be sorted by lower bound in ascending order"
              else if (prev.isOverlapping(next)) "intervals must not overlap"
              else "intervals must follow each other with a gap (adjacent intervals should be merged)"
            throw ValidationException.invalidIntervalsSeq(prevStr, nextStr, index, causeStr)
          }
        case _ => 
          val show = domainOps.showOps.intervalShow
          val causeStr = "interval must be non-empty"
          if (prev.isEmpty) {
            val intervalStr = show.show(prev)
            throw ValidationException.invalidInterval(intervalStr, index - 1, causeStr)
          }
          else {
            val intervalStr = show.show(next)
            throw ValidationException.invalidInterval(intervalStr, index, causeStr)
          }
      }
  }
}
