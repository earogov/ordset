package ordset.core.segmentSeq.map

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.interval.{Interval, IntervalRelation}
import ordset.core.segmentSeq.validation.{ValidationPredicate, ValidationException, ValidatingIterable}
import ordset.core.segmentSeq.set.OrderedSetBuilderIterable.IntervalBoundsValidation
import cats.syntax.validated
import cats.syntax.show
import cats.implicits

object OrderedMapBuilderIterable {
  
  /**
   * Returns iterable of interval relations to construct ordered map.
   * 
   * Iterable provides default validation for ordered maps:
   * 
   * <div>1. All intervals must be non-empty.                                                                  </div>
   * 
   * <div>2. Lower and upper bounds of each interval `i` must be between domain bounds                         </div>
   * <div>   according to domain order:                                                                        </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.lower` `≤` `domainOps.upperBound`)                                 </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.upper` `≤` `domainOps.upperBound`)                                 </div>
   * 
   * <div>3. Value of each interval relation must not be equal to specified `defaultValue`                     </div>
   * <div>   according to `valueOps` order.                                                                    </div>
   * 
   * <div>4. For each pair of intervals `prev` and `next` must be satisfied one of conditions:                 </div>
   * 
   * <div>4.a. (`prev.upper.flipLimited` `<` `next.lower`) according to domain order                           </div>
   * <div>     (see [[ExtendedBound.flipLimited]]).                                                            </div>
   * <div>     I.e. `next` interval must follow after `prev`, and there must be a gap between them.            </div>
   * 
   * <div>4.b. (`prev.upper.flipLimited` `==` `next.lower`) according to domain order AND                      </div>
   * <div>     (prev.value `!=` next.value) according to `valueOps` order.                                     </div>
   * <div>     I.e. adjacent intervals must have different values.                                             </div>
   * <div>     Adjacent Intervals with the same values can always be merged into one.                          </div>
   */
  def default[E, D[X] <: Domain[X], V](
    iterable: Iterable[IntervalRelation[E, D, V]],
    defaultValue: V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  ): Default[E, D, V] =
    new Default(iterable, defaultValue, domainOps, valueOps)

  /**
   * Iterable of interval relations to construct ordered map.
   * 
   * Iterable provides default validation for ordered maps:
   * 
   * <div>1. All intervals must be non-empty.                                                                  </div>
   * 
   * <div>2. Lower and upper bounds of each interval `i` must be between domain bounds                         </div>
   * <div>   according to domain order:                                                                        </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.lower` `≤` `domainOps.upperBound`)                                 </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.upper` `≤` `domainOps.upperBound`)                                 </div>
   * 
   * <div>3. Value of each interval relation must not be equal to specified `defaultValue`                     </div>
   * <div>   according to `valueOps` order.                                                                    </div>
   * 
   * <div>4. For each pair of intervals `prev` and `next` must be satisfied one of conditions:                 </div>
   * 
   * <div>4.a. (`prev.upper.flipLimited` `<` `next.lower`) according to domain order                           </div>
   * <div>     (see [[ExtendedBound.flipLimited]]).                                                            </div>
   * <div>     I.e. `next` interval must follow after `prev`, and there must be a gap between them.            </div>
   * 
   * <div>4.b. (`prev.upper.flipLimited` `==` `next.lower`) according to domain order AND                      </div>
   * <div>     (prev.value `!=` next.value) according to `valueOps` order.                                     </div>
   * <div>     I.e. adjacent intervals must have different values.                                             </div>
   * <div>     Adjacent Intervals with the same values can always be merged into one.                          </div>
   */
  final class Default[E, D[X] <: Domain[X], V](
    private val iterable: Iterable[IntervalRelation[E, D, V]],
    private val defaultValue: V,
    private val domainOps: DomainOps[E, D],
    private val valueOps: ValueOps[V]
  ) extends ValidatingIterable.ValidatingIterableArity1And2[IntervalRelation[E, D, V]](
    iterable,
    new IntervalRelationBoundsValidation(domainOps, valueOps).and(
      new IntervalRelationValueValidation(defaultValue, domainOps, valueOps),
    ),
    new AdjacentIntervalRelationsValidation(domainOps, valueOps)
  )

  /**
   * Validation predicate for single interval relation. Returns `true`, iff:
   *  
   * <div>1. Interval is non-empty.                                                                            </div>
   * 
   * <div>2. Lower and upper bounds of interval `i` are between domain bounds according to domain order:       </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.lower` `≤` `domainOps.upperBound`)                                 </div>
   * <div>   (`domainOps.lowerBound` `≤` `i.upper` `≤` `domainOps.upperBound`)                                 </div>
   */
  final class IntervalRelationBoundsValidation[E, D[X] <: Domain[X], V](
    private val domainOps: DomainOps[E, D],
    private val valueOps: ValueOps[V]
  ) extends ValidationPredicate.Arity1[IntervalRelation[E, D, V]] {

    override def apply(x: IntervalRelation[E, D, V]): Boolean = 
      x.interval match {
        case interval: Interval.NonEmpty[E, D] => 
          domainOps.containsExtended(interval.lower) && domainOps.containsExtended(interval.upper)
        case _ => 
          false
      }

    @throws[ValidationException]("if validation is failed")
    override def validate(x: IntervalRelation[E, D, V], index: Long): Unit = 
      x.interval match {
        case interval: Interval.NonEmpty[E, D] => 
          val invalidBound: String | Null = 
            if (!domainOps.containsExtended(interval.lower)) "lower"
            else if (!domainOps.containsExtended(interval.upper)) "upper"
            else null

          if (invalidBound != null) {
            val showOps = domainOps.showOps
            val relStr = showOps.intervalRelationShow(valueOps.valueShow).show(x)
            val boundsStr = showOps.rangeShow.show(domainOps.boundsRange)
            val causeStr = s"$invalidBound bound of interval is out of domain bounds $boundsStr"
            throw ValidationException.invalidIntervalRel(relStr, index, causeStr)
          }
        case _ =>
          val relStr = domainOps.showOps.intervalRelationShow(valueOps.valueShow).show(x)
          val causeStr = "interval must be non-empty"
          throw ValidationException.invalidIntervalRel(relStr, index, causeStr)
      }
  }

  /**
   * Validation predicate for single interval relation. Returns `true`, iff:
   *  
   * <div>
   *   1. Value of interval relation doesn't equal to specified `defaultValue` according to `valueOps` order.  
   * </div>
   */
  final class IntervalRelationValueValidation[E, D[X] <: Domain[X], V](
    private val defaultValue: V,
    private val domainOps: DomainOps[E, D],
    private val valueOps: ValueOps[V]
  ) extends ValidationPredicate.Arity1[IntervalRelation[E, D, V]] {

    override def apply(x: IntervalRelation[E, D, V]): Boolean =
      valueOps.neqv(x.value, defaultValue)

    @throws[ValidationException]("if validation is failed")
    override def validate(x: IntervalRelation[E, D, V], index: Long): Unit = 
      if (!apply(x)) {
        val valShow = valueOps.valueShow
        val relShow = domainOps.showOps.intervalRelationShow(valShow)
        val relStr = relShow.show(x)
        val defaultValStr = valShow.show(defaultValue)
        val causeStr = s"value equals to the default value $defaultValStr (such intervals should be dropped)"
        throw ValidationException.invalidIntervalRel(relStr, index, causeStr)
      }
  }

  /**
   * Validation predicate for pair of interval relations (`prev`, `next`). Returns `true`, iff:
   *  
   * <div>1. Both `prev` and `next` are non-empty intervals.                                                   </div>
   * 
   * <div>2. One of conditions is satisfied:                                                                   </div>
   * 
   * <div>2.a. (`prev.upper.flipLimited` `<` `next.lower`) according to domain order                           </div>
   * <div>     (see [[ExtendedBound.flipLimited]]).                                                            </div>
   * <div>     I.e. `next` interval must follow after `prev`, and there must be a gap between them.            </div>
   * 
   * <div>2.b. (`prev.upper.flipLimited` `==` `next.lower`) according to domain order AND                      </div>
   * <div>     (prev.value `!=` next.value) according to `valueOps` order.                                     </div>
   * <div>     I.e. adjacent intervals must have different values.                                             </div>
   * <div>     Adjacent Intervals with the same values can always be merged into one.                          </div>
   */
  final class AdjacentIntervalRelationsValidation[E, D[X] <: Domain[X], V](
    private val domainOps: DomainOps[E, D],
    private val valueOps: ValueOps[V]
  ) extends ValidationPredicate.Arity2[IntervalRelation[E, D, V]] {

    override def apply(prev: IntervalRelation[E, D, V], next: IntervalRelation[E, D, V]): Boolean = 
      (prev.interval, next.interval) match {
        case (prevInt: Interval.NonEmpty[E, D], nextInt: Interval.NonEmpty[E, D]) =>
          if (prevInt.isAdjacentPrecedingNE(nextInt)) valueOps.neqv(prev.value, next.value)
          else prevInt.isSeparatedPrecedingNE(nextInt)
        case _ =>
          false
      }

    @throws[ValidationException]("if validation is failed")
    override def validate(prev: IntervalRelation[E, D, V], next: IntervalRelation[E, D, V], index: Long): Unit = 
      (prev.interval, next.interval) match {
        case (prevInt: Interval.NonEmpty[E, D], nextInt: Interval.NonEmpty[E, D]) =>
          // (*1)
          if (prevInt.isAdjacentPrecedingNE(nextInt)) {
            if (valueOps.eqv(prev.value, next.value)) {
              val ord = domainOps.extendedOrd
              val show = domainOps.showOps.intervalRelationShow(valueOps.valueShow)
              val prevStr = show.show(prev)
              val nextStr = show.show(next)
              val causeStr = "adjacent intervals must have different values"
              throw ValidationException.invalidIntervalRelSeq(prevStr, nextStr, index, causeStr)
            }
          // (*2)
          } else if (!prevInt.isSeparatedPrecedingNE(nextInt)) {
            val ord = domainOps.extendedOrd
            val show = domainOps.showOps.intervalRelationShow(valueOps.valueShow)
            val prevStr = show.show(prev)
            val nextStr = show.show(next)
            val causeStr = 
              // (*3)
              if (ord.lteqv(nextInt.lower, prevInt.lower)) "intervals must be sorted by lower bound in ascending order"
              // The following cases has been excluded already:
              //     
              //    prevInt     nextInt
              // [----------)[----------]  - (*1) prevInt.isAdjacentPrecedingNE(nextInt) == true
              //
              //   prevInt      nextInt
              // [--------)  [----------]  - (*2) prevInt.isSeparatedPrecedingNE(nextInt) == true
              //
              //    nextInt
              // [----------]  prevInt
              //      (-----------------]  - (*3) ord.lteqv(nextInt.lower, prevInt.lower) == true
              //
              // So the only possible case is overlapping:
              //  
              //    prevInt
              // [----------]  nextInt
              //      (-----------------]  - prevInt.isOverlapping(nextInt) == true 
              else "intervals must not overlap"
            throw ValidationException.invalidIntervalRelSeq(prevStr, nextStr, index, causeStr)
          }
        case _ @ (prevInt, nextInt) =>
          val show = domainOps.showOps.intervalRelationShow(valueOps.valueShow)
          val causeStr = "interval must be non-empty"
          if (prevInt.isEmpty) {
            val relStr = show.show(prev)
            throw ValidationException.invalidIntervalRel(relStr, index - 1, causeStr)
          }
          else {
            val relStr = show.show(next)
            throw ValidationException.invalidIntervalRel(relStr, index, causeStr)
          }
      }
  }
}
