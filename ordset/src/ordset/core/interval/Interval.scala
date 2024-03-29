package ordset.core.interval

import ordset.core.{Bound, ExtendedBound}
import ordset.core.segmentSeq.SetBuilderFormat
import ordset.core.domain.Domain
import ordset.core.range.Range
import ordset.{Hash, Show, util}

import scala.{specialized => sp}

sealed trait Interval[E, D[X] <: Domain[X]] extends Range[ExtendedBound[E]] {

  /** Domain of ordered elements. */
  def domain: D[E]

  /** @return `true` if interval is empty, i.e. contains no elements. */
  override def isEmpty: Boolean

  /** @return `true` if interval is non-empty, i.e. contains some elements. */
  override def isNonEmpty: Boolean

  /** @return `true` if interval has both lower and upper bounds. */
  def isBounded: Boolean

  /** @return `true` if interval has lower bound. */
  def isBoundedBelow: Boolean

  /** @return `true` if interval has upper bound. */
  def isBoundedAbove: Boolean

  /** @return `true` if `bound` is between interval bounds. */
  def containsBound(bound: Bound[E]): Boolean

  /** @return `true` if `bound` is between interval bounds. */
  def containsExtended(bound: ExtendedBound[E]): Boolean

  /** @return `true` if `element` is between interval bounds. */
  def containsElement(element: E): Boolean = containsBound(Bound.Upper.including(element))

  /** @return `true` if interval has specified lower bound. */
  def hasLowerBound(bound: Bound.Lower[E]): Boolean

  /** @return `true` if interval has specified extended lower bound. */
  def hasLowerExtended(bound: ExtendedBound.Lower[E]): Boolean =
    bound match {
      case b: Bound.Lower[E] => hasLowerBound(b)
      case ExtendedBound.BelowAll => !isBoundedBelow && !isEmpty
    }

  /** @return `true` if interval has specified upper bound. */
  def hasUpperBound(bound: Bound.Upper[E]): Boolean

  /** @return `true` if interval has specified extended upper bound. */
  def hasUpperExtended(bound: ExtendedBound.Upper[E]): Boolean =
    bound match {
      case b: Bound.Upper[E] => hasUpperBound(b)
      case ExtendedBound.AboveAll => !isBoundedAbove && !isEmpty
    }

  /**
   * Returns `true`, if conditions are satisfied:
   * <div>
   *   - `this` interval and `other` are non-empty.
   * </div>
   * <div>
   *   - `this` interval follows before `other` without gap and overlapping,
   *     i.e. there are no elements between intervals and no elements, that belong to both intervals.
   * </div>  
   * {{{
   * 
   *     1. this.isAdjacentPreceding(other) = true
   * 
   *         `this`      `other`
   *     [----------)[------------]
   * 
   *     2. this.isAdjacentPreceding(other) = false
   * 
   *         `other`     `this`
   *     [----------)[------------]
   * 
   *     3. this.isAdjacentPreceding(other) = false
   *          
   *       `this`        `other`
   *     [--------]  (------------]
   * 
   *     4. this.isAdjacentPreceding(other) = false
   * 
   *          `this`          
   *     [-------------]  `other`
   *               (--------------]
   * }}}
   */
  def isAdjacentPreceding(other: Interval[E, D]): Boolean

  /**
   * Same as [[isAdjacentPreceding]] with non-empty `other` interval.
   */
  def isAdjacentPrecedingNE(other: Interval.NonEmpty[E, D]): Boolean

  /**
   * Returns `true`, if conditions are satisfied:
   * <div>
   *   - `this` interval and `other` are non-empty.
   * </div>
   * <div>
   *   - intervals follow each other without gap and overlapping,
   *     i.e. there are no elements between intervals and no elements, that belong to both intervals.
   * </div> 
   * {{{
   * 
   *     1. this.isAdjacent(other) = true
   * 
   *         `this`      `other`
   *     [----------)[------------]
   * 
   *     2. this.isAdjacent(other) = true
   * 
   *         `other`     `this`
   *     [----------)[------------]
   * 
   *     3. this.isAdjacent(other) = false
   *          
   *       `this`        `other`
   *     [--------]  (------------]
   * 
   *     4. this.isAdjacent(other) = false
   * 
   *          `this`          
   *     [-------------]  `other`
   *               (--------------]
   * }}}
   */
  def isAdjacent(other: Interval[E, D]): Boolean

  /**
   * Same as [[isAdjacent]] with non-empty `other` interval.
   */
  def isAdjacentNE(other: Interval.NonEmpty[E, D]): Boolean

  /**
   * Returns `true`, if conditions are satisfied:
   * <div>
   *   - `this` interval and `other` are non-empty.
   * </div>
   * <div>
   *   - `this` interval follows before `other` with a gap, i.e. there are some elements between intervals.
   * </div> 
   * {{{
   * 
   *     1. this.isSeparatedPreceding(other) = true
   *          
   *       `this`        `other`
   *     [--------]  (------------]
   * 
   *     2. this.isSeparatedPreceding(other) = false
   * 
   *         `this`      `other`
   *     [----------)[------------]
   * 
   *     3. this.isSeparatedPreceding(other) = false
   * 
   *          `this`          
   *     [-------------]  `other`
   *               (--------------]
   * 
   *     4. this.isSeparatedPreceding(other) = false
   * 
   *        `other`       `this`
   *     [----------)  [----------]
   * }}}
   */
  def isSeparatedPreceding(other: Interval[E, D]): Boolean

  /**
   * Same as [[isSeparatedPreceding]] with non-empty `other` interval.
   */
  def isSeparatedPrecedingNE(other: Interval.NonEmpty[E, D]): Boolean

  /**
   * Returns `true`, if conditions are satisfied:
   * <div>
   *   - `this` interval and `other` are non-empty.
   * </div>
   * <div>
   *   - there is a gap between `this` and `other` intervals, i.e. there are some elements between them.
   * </div> 
   * {{{
   * 
   *     1. this.isSeparatedPreceding(other) = true
   *          
   *       `this`        `other`
   *     [--------]  (------------]
   * 
   *     2. this.isSeparatedPreceding(other) = false
   * 
   *         `this`      `other`
   *     [----------)[------------]
   * 
   *     3. this.isSeparatedPreceding(other) = false
   * 
   *          `this`          
   *     [-------------]  `other`
   *               (--------------]
   * 
   *     4. this.isSeparatedPreceding(other) = true
   * 
   *        `other`       `this`
   *     [----------)  [----------]
   * }}}
   */
  def isSeparated(other: Interval[E, D]): Boolean

  /**
   * Same as [[isSeparated]] with non-empty `other` interval.
   */
  def isSeparatedNE(other: Interval.NonEmpty[E, D]): Boolean

  /**
   * Returns `true`, if conditions are satisfied:
   * <div>
   *   - `this` interval and `other` are non-empty.
   * </div>
   * <div>
   *   - there are some elements, that belongs to both `this` and `other` intervals.
   * </div>
   * {{{
   * 
   *     1. this.isOverlapping(other) = true
   * 
   *          `this`          
   *     [-------------]  `other`
   *               (--------------]
   * 
   *     2. this.isOverlapping(other) = true
   * 
   *                `this`          
   *     [------------------------]  
   *                `other`
   *           (--------------]
   * 
   *     3. this.isOverlapping(other) = true
   * 
   *                `other`          
   *     [------------------------]  
   *                `this`
   *           (--------------]
   * 
   *     4. this.isOverlapping(other) = true
   * 
   *          `other`          
   *     [-------------]  `this`
   *               (--------------]
   * 
   *     5. this.isOverlapping(other) = false
   * 
   *        `this`      `other`
   *     [----------)[------------]
   * 
   *     6. this.isOverlapping(other) = false
   * 
   *        `this`       `other`
   *     [----------)  [----------]
   * }}}
   */
  def isOverlapping(other: Interval[E, D]): Boolean

  /**
   * Same as [[isOverlapping]] with non-empty `other` interval.
   */
  def isOverlappingNE(other: Interval.NonEmpty[E, D]): Boolean

  /** @return [[IntervalRelation]] for current interval and specified `value`. */
  def ->[@sp(Boolean) V](value: V): IntervalRelation[E, D, V] = IntervalRelation(this, value)
}

object Interval {

  implicit def defaultHash[E, D[X] <: Domain[X]](
    implicit boundHash: Hash[Bound[E]]
  ): DefaultHash[E, D] =
    new DefaultHashImpl(boundHash)

  implicit def defaultShow[E, D[X] <: Domain[X]](
    implicit elementShow: Show[E]
  ): Show[Interval[E, D]] =
    SetBuilderFormat.intervalShow(elementShow)

  sealed trait NonEmpty[E, D[X] <: Domain[X]] extends Interval[E, D] with Range.NonEmpty[ExtendedBound[E]] {

    override def lower: ExtendedBound.Lower[E]

    override def upper: ExtendedBound.Upper[E]

    /**
     * Returns input `bound`, if it is inside interval, otherwise returns interval bound closest to input `bound` 
     * (either lower or upper).
     *
     * <h3>Note</h3>
     *
     * The formula below seems to be equivalent to method definition:
     *
     * output bound = min(max(`bound`, lower bound of interval), upper bound of interval)     (1)
     *
     * But there is a subtle difference: according to bound ordering defined by domain two bounds,
     * for example, 5`]` and `[`5 are equal. So min() and max() operators can return any of them.
     *
     * Consider the case.
     * {{{
     *       bound
     *         ]
     *         [-----------)
     *         5     ^     10
     *            interval
     * }}}
     * [[restrictBound]] must return `bound` = 5`]`.
     * But implementation based on formula (1) can return either 5`]` or 5`[`.
     */
    def restrictBound(bound: Bound[E]): Bound[E]

    /**
     * Returns input extended `bound`, if it is inside interval, otherwise returns interval bound closest to
     * input `bound` (either lower or upper).
     *
     * @see [[restrictBound]]
     */
    def restrictExtended(bound: ExtendedBound[E]): ExtendedBound[E] =
      bound match {
        case b: Bound[E] => restrictBound(b)
        case ExtendedBound.BelowAll => lower
        case ExtendedBound.AboveAll => upper
      }
  }

  sealed trait BoundedBelow[E, D[X] <: Domain[X]] extends NonEmpty[E, D] {

    override def lower: Bound.Lower[E]

    override def isBoundedBelow: Boolean = true

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = domain.boundOrd.eqv(lower, bound)
  }

  sealed trait BoundedAbove[E, D[X] <: Domain[X]] extends NonEmpty[E, D] {

    override def upper: Bound.Upper[E]

    override def isBoundedAbove: Boolean = true

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = domain.boundOrd.eqv(upper, bound)

    override def isAdjacentPreceding(other: Interval[E, D]): Boolean =
      other match {
        case other: Interval.NonEmpty[E, D] => isAdjacentPrecedingNE(other)
        case _ => false
      }

    override def isAdjacentPrecedingNE(other: Interval.NonEmpty[E, D]): Boolean =
      domain.extendedOrd.eqv(upper.flip, other.lower)

    override def isSeparatedPreceding(other: Interval[E, D]): Boolean =
      other match {
        case other: Interval.NonEmpty[E, D] => isSeparatedPrecedingNE(other)
        case _ => false
      }

    override def isSeparatedPrecedingNE(other: Interval.NonEmpty[E, D]): Boolean = 
      domain.extendedOrd.lt(upper.flip, other.lower)
  }

  final case class Empty[E, D[X] <: Domain[X]](
    override val domain: D[E]
  ) extends Interval[E, D] with Range.Empty {

    override def isBounded: Boolean = false

    override def isBoundedBelow: Boolean = false

    override def isBoundedAbove: Boolean = false

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = false

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = false

    override def containsBound(bound: Bound[E]): Boolean = false

    override def containsExtended(bound: ExtendedBound[E]): Boolean = false

    override def isAdjacentPreceding(other: Interval[E, D]): Boolean = false

    override def isAdjacentPrecedingNE(other: Interval.NonEmpty[E, D]): Boolean = false

    override def isAdjacent(other: Interval[E, D]): Boolean = false

    override def isAdjacentNE(other: Interval.NonEmpty[E, D]): Boolean = false

    override def isSeparatedPreceding(other: Interval[E, D]): Boolean = false

    override def isSeparatedPrecedingNE(other: Interval.NonEmpty[E, D]): Boolean = false

    override def isSeparated(other: Interval[E, D]): Boolean = false

    override def isSeparatedNE(other: Interval.NonEmpty[E, D]): Boolean = false

    override def isOverlapping(other: Interval[E, D]): Boolean = false

    override def isOverlappingNE(other: Interval.NonEmpty[E, D]): Boolean = false

    override def toString: String = SetBuilderFormat.emptyInterval
  }

  final case class Unbounded[E, D[X] <: Domain[X]](
    override val domain: D[E]
  ) extends NonEmpty[E, D] {

    override def lower: ExtendedBound.BelowAll.type = ExtendedBound.BelowAll

    override def upper: ExtendedBound.AboveAll.type = ExtendedBound.AboveAll

    override def isBounded: Boolean = false

    override def isBoundedBelow: Boolean = false

    override def isBoundedAbove: Boolean = false

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = false

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = false

    override def containsBound(bound: Bound[E]): Boolean = true

    override def containsExtended(bound: ExtendedBound[E]): Boolean = true

    override def restrictBound(bound: Bound[E]): Bound[E] = bound

    override def restrictExtended(bound: ExtendedBound[E]): ExtendedBound[E] = bound

    override def isAdjacentPreceding(other: Interval[E, D]): Boolean = false

    override def isAdjacentPrecedingNE(other: Interval.NonEmpty[E, D]): Boolean = false

    override def isAdjacent(other: Interval[E, D]): Boolean = false

    override def isAdjacentNE(other: Interval.NonEmpty[E, D]): Boolean = false

    override def isSeparatedPreceding(other: Interval[E, D]): Boolean = false

    override def isSeparatedPrecedingNE(other: Interval.NonEmpty[E, D]): Boolean = false

    override def isSeparated(other: Interval[E, D]): Boolean = false

    override def isSeparatedNE(other: Interval.NonEmpty[E, D]): Boolean = false

    override def isOverlapping(other: Interval[E, D]): Boolean = other.isNonEmpty

    override def isOverlappingNE(other: Interval.NonEmpty[E, D]): Boolean = true

    override def toString: String = SetBuilderFormat.unboundedInterval
  }

  final case class Greater[E, D[X] <: Domain[X]](
    override val lower: Bound.Lower[E],
    override val domain: D[E]
  ) extends BoundedBelow[E, D] {

    override def upper: ExtendedBound.AboveAll.type = ExtendedBound.AboveAll

    override def isBounded: Boolean = false

    override def isBoundedAbove: Boolean = false

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = false

    override def containsBound(bound: Bound[E]): Boolean = domain.boundOrd.lteqv(lower, bound)

    override def containsExtended(bound: ExtendedBound[E]): Boolean =
      bound match {
        case b: Bound[E] => containsBound(b)
        case ExtendedBound.BelowAll => false
        case ExtendedBound.AboveAll => true
      }

    override def restrictBound(bound: Bound[E]): Bound[E] =
      if (domain.boundOrd.lt(bound, lower)) lower
      else bound

    override def isAdjacentPreceding(other: Interval[E, D]): Boolean = false

    override def isAdjacentPrecedingNE(other: Interval.NonEmpty[E, D]): Boolean = false

    override def isAdjacent(other: Interval[E, D]): Boolean = other.isAdjacentPreceding(this)

    override def isAdjacentNE(other: Interval.NonEmpty[E, D]): Boolean = other.isAdjacentPrecedingNE(this)

    override def isSeparatedPreceding(other: Interval[E, D]): Boolean = false

    override def isSeparatedPrecedingNE(other: Interval.NonEmpty[E, D]): Boolean = false

    override def isSeparated(other: Interval[E, D]): Boolean = other.isSeparatedPreceding(this)

    override def isSeparatedNE(other: Interval.NonEmpty[E, D]): Boolean = other.isSeparatedPrecedingNE(this)

    override def isOverlapping(other: Interval[E, D]): Boolean =
      other match {
        case other: Interval.NonEmpty[E, D] => isOverlappingNE(other)
        case _ => false
      }

    override def isOverlappingNE(other: Interval.NonEmpty[E, D]): Boolean =
      domain.extendedOrd.lteqv(lower, other.upper)

    override def toString: String = SetBuilderFormat.lowerBoundedInterval(this, SetBuilderFormat.toStringFunc[E])
  }

  final case class Less[E, D[X] <: Domain[X]](
    override val upper: Bound.Upper[E],
    override val domain: D[E]
  ) extends BoundedAbove[E, D] {

    override def lower: ExtendedBound.BelowAll.type = ExtendedBound.BelowAll

    override def isBounded: Boolean = false

    override def isBoundedBelow: Boolean = false

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = false

    override def containsBound(bound: Bound[E]): Boolean = domain.boundOrd.gteqv(upper, bound)

    override def containsExtended(bound: ExtendedBound[E]): Boolean =
      bound match {
        case b: Bound[E] => containsBound(b)
        case ExtendedBound.BelowAll => true
        case ExtendedBound.AboveAll => false
      }

    override def restrictBound(bound: Bound[E]): Bound[E] =
      if (domain.boundOrd.gt(bound, upper)) upper
      else bound

    override def isAdjacent(other: Interval[E, D]): Boolean = isAdjacentPreceding(other)

    override def isAdjacentNE(other: Interval.NonEmpty[E, D]): Boolean = isAdjacentPrecedingNE(other)

    override def isSeparated(other: Interval[E, D]): Boolean = isSeparatedPreceding(other)

    override def isSeparatedNE(other: Interval.NonEmpty[E, D]): Boolean = isSeparatedPrecedingNE(other)

    override def isOverlapping(other: Interval[E, D]): Boolean =
      other match {
        case other: Interval.NonEmpty[E, D] => isOverlappingNE(other)
        case _ => false
      }

    override def isOverlappingNE(other: Interval.NonEmpty[E, D]): Boolean =
      domain.extendedOrd.lteqv(other.lower, upper)

    override def toString: String = SetBuilderFormat.upperBoundedInterval(this, SetBuilderFormat.toStringFunc[E])
  }

  final case class Between[E, D[X] <: Domain[X]](
    override val lower: Bound.Lower[E],
    override val upper: Bound.Upper[E],
    override val domain: D[E]
  ) extends BoundedBelow[E, D] with BoundedAbove[E, D] with Range[Bound[E]] {

    override def isBounded: Boolean = true

    override def containsBound(bound: Bound[E]): Boolean = {
      val boundOrd = domain.boundOrd
      boundOrd.lteqv(lower, bound) && boundOrd.gteqv(upper, bound)
    }

    override def containsExtended(bound: ExtendedBound[E]): Boolean =
      bound match {
        case b: Bound[E] => containsBound(b)
        case _ => false
      }

    override def restrictBound(bound: Bound[E]): Bound[E] = {
      val boundOrd = domain.boundOrd
      if (boundOrd.lt(bound, lower)) lower
      else if (boundOrd.gt(bound, upper)) upper
      else bound
    }

    override def isAdjacent(other: Interval[E, D]): Boolean = 
      isAdjacentPreceding(other) || other.isAdjacentPrecedingNE(this)

    override def isAdjacentNE(other: Interval.NonEmpty[E, D]): Boolean =
      isAdjacentPrecedingNE(other) || other.isAdjacentPrecedingNE(this)

    override def isSeparated(other: Interval[E, D]): Boolean = 
      isSeparatedPreceding(other) || other.isSeparatedPrecedingNE(this)

    override def isSeparatedNE(other: Interval.NonEmpty[E, D]): Boolean = 
      isSeparatedPrecedingNE(other) || other.isSeparatedPrecedingNE(this)

    override def isOverlapping(other: Interval[E, D]): Boolean =
      other match {
        case other: Interval.NonEmpty[E, D] => isOverlappingNE(other)
        case _ => false
      }

    override def isOverlappingNE(other: Interval.NonEmpty[E, D]): Boolean = {
      val extendedOrd = domain.extendedOrd
      extendedOrd.lteqv(other.lower, upper) && extendedOrd.lteqv(lower, other.upper)
    }

    override def toString: String = SetBuilderFormat.boundedInterval(this, SetBuilderFormat.toStringFunc[E])
  }

  trait DefaultHash[E, D[X] <: Domain[X]] extends Hash[Interval[E, D]] {

    import util.HashUtil._

    def boundHash: Hash[Bound[E]]

    override def hash(x: Interval[E, D]): Int = x match {
      case x: Empty[e, d] => 
        0xA1F63D02
      case x: Unbounded[e, d] => 
        0x13E0FF65
      case x: Greater[e, d] => 
        product1Hash(boundHash.hash(x.lower))
      case x: Less[e, d] => 
        product1Hash(boundHash.hash(x.upper))
      case x: Between[e, d] => 
        product2Hash(boundHash.hash(x.lower), boundHash.hash(x.upper))
    }

    override def eqv(x: Interval[E, D], y: Interval[E, D]): Boolean =
      x match {
        case x: Empty[e, d] => y match {
          case _: Empty[e, d] => true
          case _ => false
        }
        case x: Unbounded[e, d] => y match {
          case _: Unbounded[e, d] => true
          case _ => false
        }
        case x: Greater[e, d] => y match {
          case y: Greater[e, d] => boundHash.eqv(x.lower: Bound.Lower[E], y.lower)
          case _ => false
        }
        case x: Less[e, d] => y match {
          case y: Less[e, d] => boundHash.eqv(x.upper, y.upper)
          case _ => false
        }
        case x: Between[e, d] => y match {
          case y: Between[e, d] => 
            boundHash.eqv(x.lower, y.lower) && boundHash.eqv(x.upper, y.upper)
          case _ => false
        }
      }
  }

  case class DefaultHashImpl[E, D[X] <: Domain[X]](
    override val boundHash: Hash[Bound[E]]
  ) extends DefaultHash[E, D]
}
