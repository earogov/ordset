package ordset.core.interval

import ordset.ContravariantOrder
import ordset.OrderExtensions.*
import ordset.core.{Bound, ExtendedBound}
import ordset.core.domain.Domain
import ordset.core.range

/**
 * Factory to construct intervals.
 */
trait IntervalFactory[E, D <: Domain[E]] {

  /**
   * Returns range factory for intervals.
   */
  lazy val asRangeFactory: IntervalFactory.RangeFactory[E, D] = new IntervalFactory.RangeFactory(this)

  /**
   * Domain of interval.
   */
  def domain: D

  /**
   * Returns empty interval, which contains no elements of domain.
   */
  def empty: Interval.Empty[E, D]

  /**
   * Returns universal interval, which contains all elements of domain.
   */
  def universal: Interval.NonEmpty[E, D]

  /**
   * Returns interval, which contains all elements of domain less than specified `upper` element.
   * If `isIncluding` is `true`, then `upper` element is included in interval.
   */
  def belowElement(
    upper: E, 
    isIncluding: Boolean
  ): Interval.Empty[E, D] | Interval.BoundedAbove[E, D] =
    belowBound(Bound.Upper(upper, isIncluding))

  /**
   * Returns interval, which contains all elements of domain greater than specified `lower` element.
   * If `isIncluding` is `true`, then `lower` element is included in interval.
   */
  def aboveElement(
    lower: E, 
    isIncluding: Boolean
  ): Interval.Empty[E, D] | Interval.BoundedBelow[E, D] =
    aboveBound(Bound.Lower(lower, isIncluding))

  /**
   * Returns interval, which contains all elements of domain greater than specified `lower` element and 
   * less than `upper` element. If `isLowerIncluding` is `true`, then `lower` element is included in interval.
   * If `isUpperIncluding` is `true`, then `upper` element is included in interval.
   */
  def betweenElements(
    lower: E, 
    isLowerIncluding: Boolean, 
    upper: E, 
    isUpperIncluding: Boolean
  ): Interval.Empty[E, D] | Interval.Between[E, D] =
    betweenBounds(Bound.Lower(lower, isLowerIncluding), Bound.Upper(upper, isUpperIncluding))

  /**
   * Returns interval, which contains all elements of domain below specified `upper` bound.
   */
  def belowBound(upper: Bound.Upper[E]): Interval.Empty[E, D] | Interval.BoundedAbove[E, D]

  /**
   * Returns interval, which contains all elements of domain above specified `lower` bound.
   */
  def aboveBound(lower: Bound.Lower[E]): Interval.Empty[E, D] | Interval.BoundedBelow[E, D]

  /**
   * Returns interval, which contains all elements of domain above specified `lower` bound and 
   * below `upper` bound.
   */
  def betweenBounds(lower: Bound.Lower[E], upper: Bound.Upper[E]): Interval.Empty[E, D] | Interval.Between[E, D]

  /**
   * Returns interval, which contains all elements of domain below specified `upper` extended bound.
   */
  def belowExtended(upper: ExtendedBound.Upper[E]): Interval[E, D] = 
    upper match {
      case b: Bound.Upper[E] => belowBound(b)
      case _ => universal // bound is `ExtendedBound.AboveAll`
    }

  /**
   * Returns interval, which contains all elements of domain above specified `lower` extended bound.
   */
  def aboveExtended(lower: ExtendedBound.Lower[E]): Interval[E, D] = 
    lower match {
      case b: Bound.Lower[E] => aboveBound(b)
      case _ => universal // bound is `ExtendedBound.BelowAll`
    }

  /**
   * Returns interval, which contains all elements of domain above specified `lower` extended bound and 
   * below `upper` extended bound.
   */
  def betweenExtended(lower: ExtendedBound.Lower[E], upper: ExtendedBound.Upper[E]): Interval[E, D] = 
    (lower, upper) match {
      case (l: Bound.Lower[E], u: Bound.Upper[E]) => betweenBounds(l, u)
      case (_, u: Bound.Upper[E]) => belowBound(u) // lower bound is `ExtendedBound.BelowAll`
      case (l: Bound.Lower[E], _) => aboveBound(l) // upper bound is `ExtendedBound.AboveAll`
      case _ => universal                          // bounds are `ExtendedBound.BelowAll` and `ExtendedBound.AboveAll`
    }
}

object IntervalFactory {

  /**
   * Returns interval factory depending on domain type (unbounded, bounded, etc).
   */
  implicit def defaultFactory[E, D <: Domain[E]](implicit domain: D): IntervalFactory[E, D] =
    domain match {
      case d: Domain.Unbounded[E] => unboundedFactory(d)
      case d: Domain.BoundedBelow[E] => boundedBelowFactory(d)
      case d: Domain.BoundedAbove[E] => boundedAboveFactory(d)
      case d: Domain.Bounded[E] => boundedFactory(d)
    }

  /**
   * Returns interval factory for unbounded domain.
   */
  def unboundedFactory[E, D <: Domain[E]](implicit domain: D & Domain.Unbounded[E]): UnboundedFactory[E, D] =
    new UnboundedFactory[E, D](domain)

  /**
   * Returns interval factory for bounded from below domain.
   */
  def boundedBelowFactory[E, D <: Domain[E]](implicit domain: D & Domain.BoundedBelow[E]): BoundedBelowFactory[E, D] =
    new BoundedBelowFactory[E, D](domain)

  /**
   * Returns interval factory for bounded from above domain.
   */
  def boundedAboveFactory[E, D <: Domain[E]](implicit domain: D & Domain.BoundedAbove[E]): BoundedAboveFactory[E, D] =
    new BoundedAboveFactory[E, D](domain)

  /**
   * Returns interval factory for bounded domain.
   */
  def boundedFactory[E, D <: Domain[E]](implicit domain: D & Domain.Bounded[E]): BoundedFactory[E, D] =
    new BoundedFactory[E, D](domain)

  /**
   * Factory to construct intervals on unbounded domain.
   */
  class UnboundedFactory[E, D <: Domain[E]](
    override val domain: D & Domain.Unbounded[E]
  ) extends IntervalFactory[E, D] {

    override lazy val empty: Interval.Empty[E, D] = Interval.Empty(domain)

    override lazy val universal: Interval.Unbounded[E, D] = Interval.Unbounded(domain)

    override def belowElement(upper: E, isIncluding: Boolean): Interval.Less[E, D] =
      belowBound(Bound.Upper(upper, isIncluding))

    override def aboveElement(lower: E, isIncluding: Boolean): Interval.Greater[E, D] =
      aboveBound(Bound.Lower(lower, isIncluding))

    override def betweenElements(
      lower: E, 
      isLowerIncluding: Boolean, 
      upper: E, 
      isUpperIncluding: Boolean
    ): Interval.Empty[E, D] | Interval.Between[E, D] =
      betweenBounds(Bound.Lower(lower, isLowerIncluding), Bound.Upper(upper, isUpperIncluding))

    override def belowBound(upper: Bound.Upper[E]): Interval.Less[E, D] = Interval.Less(upper, domain)

    override def aboveBound(lower: Bound.Lower[E]): Interval.Greater[E, D] = Interval.Greater(lower, domain)

    override def betweenBounds(
      lower: Bound.Lower[E], 
      upper: Bound.Upper[E]
    ): Interval.Empty[E, D] | Interval.Between[E, D] =
      //           lower     upper
      // input:       |-------|
      if domain.boundOrd.lteqv(lower, upper) then Interval.Between(lower, upper, domain)
      //           upper     lower
      // input:       |       |
      else empty
  }

  /**
   * Factory to construct intervals on bounded from below domain.
   */
  class BoundedBelowFactory[E, D <: Domain[E]](
    override val domain: D & Domain.BoundedBelow[E]
  ) extends IntervalFactory[E, D] {

    private val lowerBound: Bound.Lower[E] = domain.boundOrd.lowerBound

    override lazy val empty: Interval.Empty[E, D] = Interval.Empty(domain)

    override lazy val universal: Interval.Greater[E, D] = Interval.Greater(lowerBound, domain)

    override def belowElement(upper: E, isIncluding: Boolean): Interval.Empty[E, D] | Interval.Between[E, D] =
      belowBound(Bound.Upper(upper, isIncluding))

    override def aboveElement(lower: E, isIncluding: Boolean): Interval.Greater[E, D] =
      aboveBound(Bound.Lower(lower, isIncluding))

    override def belowBound(upper: Bound.Upper[E]): Interval.Empty[E, D] | Interval.Between[E, D] = 
      //                      upper
      // input:          -------|
      //                      lowerBound
      // bounds:                   |----
      if domain.boundOrd.lt(upper, lowerBound) then empty
      //                      upper
      // input:          -------|
      //          lowerBound
      // bounds:      |-----------------
      else Interval.Between(lowerBound, upper, domain)
      
    override def aboveBound(lower: Bound.Lower[E]): Interval.Greater[E, D] =
      //               lower
      // input:          |-------
      //                 lowerBound
      // bounds:             |----------
      if domain.boundOrd.lteqv(lower, lowerBound) then universal
      //               lower
      // input:          |-------
      //          lowerBound
      // bounds:      |-----------------
      else Interval.Greater(lower, domain)

    override def betweenBounds(
      lower: Bound.Lower[E], 
      upper: Bound.Upper[E]
    ): Interval.Empty[E, D] | Interval.Between[E, D] =
      //           upper     lower
      // input:       |       |
      if domain.boundOrd.lt(upper, lower) then empty
      //              lower    upper
      // input:          |------|
      //          lowerBound
      // bounds:      |-----------------
      else if domain.boundOrd.lteqv(lowerBound, lower) then Interval.Between(lower, upper, domain)
      //              lower    upper
      // input:          |------|
      //                lowerBound
      // bounds:             |----------
      //
      //              lower    upper
      // input:          |------|
      //                      lowerBound
      // bounds:                    |---
      else belowBound(upper)
  }

  /**
   * Factory to construct intervals on bounded from above domain.
   */
  class BoundedAboveFactory[E, D <: Domain[E]](
    override val domain: D & Domain.BoundedAbove[E]
  ) extends IntervalFactory[E, D] {

    private val upperBound: Bound.Upper[E] = domain.boundOrd.upperBound

    override lazy val empty: Interval.Empty[E, D] = Interval.Empty(domain)

    override lazy val universal: Interval.Less[E, D] = Interval.Less(upperBound, domain)

    override def belowElement(upper: E, isIncluding: Boolean): Interval.Less[E, D] =
      belowBound(Bound.Upper(upper, isIncluding))

    override def aboveElement(lower: E, isIncluding: Boolean): Interval.Empty[E, D] | Interval.Between[E, D] =
      aboveBound(Bound.Lower(lower, isIncluding))

    override def belowBound(upper: Bound.Upper[E]): Interval.Less[E, D] = 
      //                      upper
      // input:          -------|
      //                upperBound
      // bounds:      -------|
      if domain.boundOrd.lteqv(upperBound, upper) then universal
      //                      upper
      // input:          -------|
      //                       upperBound
      // bounds:      -------------|
      else Interval.Less(upper, domain)

    override def aboveBound(lower: Bound.Lower[E]): Interval.Empty[E, D] | Interval.Between[E, D] = 
      //                   lower
      // input:              |------
      //              upperBound
      // bounds:      ----|
      if domain.boundOrd.lt(upperBound, lower) then empty
      //               lower
      // input:          |-------
      //                       upperBound
      // bounds:      -------------|
      else Interval.Between(lower, upperBound, domain)

    override def betweenBounds(
      lower: Bound.Lower[E], 
      upper: Bound.Upper[E]
    ): Interval.Empty[E, D] | Interval.Between[E, D] =
      //           upper     lower
      // input:       |       |
      if domain.boundOrd.lt(upper, lower) then empty
      //              lower    upper
      // input:          |------|
      //                       upperBound
      // bounds:      -------------|
      else if domain.boundOrd.lteqv(upper, upperBound) then Interval.Between(lower, upper, domain)
      //              lower    upper
      // input:          |------|
      //                upperBound
      // bounds:      -------|
      //
      //              lower    upper
      // input:          |------|
      //          upperBound
      // bounds:      -|
      else aboveBound(lower)
  }

  /**
   * Factory to construct intervals on bounded domain.
   */
  class BoundedFactory[E, D <: Domain[E]](
    override val domain: D & Domain.Bounded[E]
  ) extends IntervalFactory[E, D] {

    private val lowerBound: Bound.Lower[E] = domain.boundOrd.lowerBound

    private val upperBound: Bound.Upper[E] = domain.boundOrd.upperBound

    override lazy val empty: Interval.Empty[E, D] = Interval.Empty(domain)

    override lazy val universal: Interval.Between[E, D] = Interval.Between(lowerBound, upperBound, domain)

    override def belowElement(upper: E, isIncluding: Boolean): Interval.Empty[E, D] | Interval.Between[E, D] =
      belowBound(Bound.Upper(upper, isIncluding))

    override def aboveElement(lower: E, isIncluding: Boolean): Interval.Empty[E, D] | Interval.Between[E, D] =
      aboveBound(Bound.Lower(lower, isIncluding))

    override def belowBound(upper: Bound.Upper[E]): Interval.Empty[E, D] | Interval.Between[E, D] = 
      //                           upper
      // input:               -------|
      //          lowerBound   upperBound
      // bounds:      |------------|
      if domain.boundOrd.lteqv(upperBound, upper) then universal
      //                   upper
      // input:       -------|
      //          lowerBound   upperBound
      // bounds:      |------------|
      else if domain.boundOrd.lteqv(lowerBound, upper) then Interval.Between(lowerBound, upper, domain)
      //          upper
      // input:  ---|
      //          lowerBound   upperBound
      // bounds:      |------------|
      else empty

    override def aboveBound(lower: Bound.Lower[E]): Interval.Empty[E, D] | Interval.Between[E, D] = 
      //         lower
      // input:    |-------
      //          lowerBound   upperBound
      // bounds:      |------------|
      if domain.boundOrd.lteqv(lower, lowerBound) then universal
      //                  lower
      // input:             |-------
      //          lowerBound   upperBound
      // bounds:      |------------|
      else if domain.boundOrd.lteqv(lower, upperBound) then Interval.Between(lower, upperBound, domain)
      //                           lower
      // input:                      |----
      //          lowerBound   upperBound
      // bounds:      |------------|
      else empty

    override def betweenBounds(
      lower: Bound.Lower[E], 
      upper: Bound.Upper[E]
    ): Interval.Empty[E, D] | Interval.Between[E, D] = 
      //           upper     lower
      // input:       |       |
      if domain.boundOrd.lt(upper, lower) then empty
      //         lower              upper
      // input:    |------------------|
      //          lowerBound   upperBound
      // bounds:      |------------|
      //
      //         lower     upper
      // input:    |---------|
      //          lowerBound   upperBound
      // bounds:      |------------|
      //
      //       lower upper
      // input:    |-|
      //          lowerBound   upperBound
      // bounds:      |------------|
      else if domain.boundOrd.lteqv(lower, lowerBound) then belowBound(upper)
      //               lower    upper
      // input:          |-------|
      //          lowerBound   upperBound
      // bounds:      |------------|
      else if domain.boundOrd.lteqv(upper, upperBound) then Interval.Between(lower, upper, domain)
      //               lower        upper
      // input:          |------------|
      //          lowerBound   upperBound
      // bounds:      |------------|
      else if domain.boundOrd.lteqv(lower, upperBound) then Interval.Between(lower, upperBound, domain)
      //                        lower upper
      // input:                     |-|
      //          lowerBound   upperBound
      // bounds:      |------------|
      else empty
  }

  /**
   * Implementation of range factory for intervals.
   */
  class RangeFactory[E, D <: Domain[E]](
    val intervalFactory: IntervalFactory[E, D]
  ) extends range.RangeFactory[ExtendedBound[E], Interval[E, D]] {

    override val order: ContravariantOrder[ExtendedBound[E]] = intervalFactory.domain.extendedOrd.contravariant

    override def empty: Interval.Empty[E, D] = Interval.Empty(intervalFactory.domain)

    /**
     * Returns interval between specified extended bounds.
     * 
     * Lower bound of interval has type [[Bound.Lower]] and upper bound - [[Bound.Upper]].
     * If input bounds `lower` and `upper` have other types, they are adjusted with [[Bound.flip]] operation:
     * <p>
     * between(Bound.Upper(0, isIncluding = false), Bound.Lower(10, isIncluding = false)) returns 
     * Interval.Between(Bound.Lower(0, isIncluding = true), Bound.Upper(10, isIncluding = true))
     * </p>
     * {{{
     * 
     *       0)          (10     - input bounds
     *         [--------]        - output interval
     * }}}
     * <p>
     * between(Bound.Upper(0, isIncluding = true), Bound.Lower(10, isIncluding = true)) returns 
     * Interval.Between(Bound.Lower(0, isIncluding = false), Bound.Upper(10, isIncluding = false))
     * </p>
     * {{{
     * 
     *       0]          [10     - input bounds
     *         (--------)        - output interval
     * }}}
     * 
     * If `lower` bound is [[ExtendedBound.AboveAll]] or `upper` bound is [[ExtendedBound.BelowAll]],
     * then [[Interval.Empty]] will be returned.
     */
    override def between(lower: ExtendedBound[E], upper: ExtendedBound[E]): Interval[E, D] =
      (lower, upper) match {
        case (lower: Bound[E], upper: Bound[E]) => intervalFactory.betweenBounds(lower.provideLower, upper.provideUpper)
        case (lower: Bound[E], ExtendedBound.AboveAll) => intervalFactory.aboveBound(lower.provideLower)
        case (ExtendedBound.BelowAll, upper: Bound[E]) => intervalFactory.belowBound(upper.provideUpper)
        case (ExtendedBound.BelowAll, ExtendedBound.AboveAll) => intervalFactory.universal
        // (AboveAll, Bound[E]) | (Bound[E], BelowAll) | (AboveAll, BelowAll)
        case _ => intervalFactory.empty
      }
  }
}