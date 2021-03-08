package ordset.core

import ordset.core.domain._
import ordset.util
import ordset.util.label.Label
import ordset.util.types.SingleValue

import scala.collection.{AbstractIterable, AbstractIterator}

/**
 * Segment is equivalent to interval with some value assigned to it.
 * The main property of segments is that they <u>cover ordered universal set without gaps and overlapping</u>.
 * So we can move from given segment to the next, previous, first or last segment of sequence.
 *
 * <h1>Basic Types</h1>
 *
 * Consider segment sequences.
 *
 * 1. General case.
 *
 * {{{
 *   Initial     Inner     Terminal
 *     v           v          v
 * X--------|-----------|-----------X
 * }}}
 *
 * 2. Special case (empty or universal sequence).
 *
 * {{{
 *               Single
 *                 v
 * X--------------------------------X
 * }}}
 * {{{
 * X - sequence bound;
 * | - segment bound.
 * }}}
 *
 * We can introduce next segment types.
 *
 * <h2>Initial</h2>
 *
 * First segment of sequence with properties:
 *
 * - doesn't have previous segment;
 *
 * - always has next segment.
 *
 * <h2>Terminal</h2>
 *
 * Last segment of sequence with properties:
 *
 * - doesn't have next segment;
 *
 * - always has previous segment.
 *
 * <h2>Inner</h2>
 *
 * Segment with properties:
 *
 * - always has next segment;
 *
 * - always has previous segment.
 *
 * <h2>Single</h2>
 *
 * The only segment in sequence (in case of empty or universal sequence) with properties:
 *
 * - doesn't have next segment;
 *
 * - doesn't have previous segment.
 *
 * <h3>
 * Each concrete class implementing segment MUST have ONE (and only one) of supertypes:
 * </h3>
 * <tr><b> - Initial  </b></tr>
 * <tr><b> - Terminal </b></tr>
 * <tr><b> - Inner    </b></tr>
 * <tr><b> - Single   </b></tr>
 *
 * <h1>Segment Hierarchy</h1>
 *
 * {{{
 *
 *                   Single
 *                 ↙        ↘
 *            First         Last
 *          ↗      ↘      ↙      ↖
 *     Initial      Segment       Terminal
 *          ↘      ↗      ↖      ↙
 *           WithNext      WithPrev
 *                 ↖      ↗
 *                   Inner
 *
 *    * subtype -> supertype
 * }}}
 *
 * General segment hierarchy include some auxiliary supertypes:
 *
 * <tr><b>Segment </b> - supertype for all segments.</tr>
 *
 * <tr><b>First   </b> - first segment of sequence. Doesn't have previous segment. May be Single or Initial.</tr>
 *
 * <tr><b>Last    </b> - last segment of sequence. Doesn't have next segment. May be Single or Terminal.</tr>
 *
 * <tr><b>WithPrev</b> - segment which has previous segment. May be Terminal or Inner.</tr>
 *
 * <tr><b>WithNext</b> - segment which has next segment. May be Initial or Inner.</tr>
 *
 * <tr></tr>
 *
 * @tparam E type of element in ordered set
 * @tparam D type of domain
 * @tparam V type of value assigned to interval
 *
 * <tr></tr>
 *
 * @note Definition of segment (traits) has forward/backward symmetry: for example if we have `moveNext` there is
 *       also `movePrev` method. But its implementation may be optimized to move forward, as it's assumed this is
 *       the basic use case of segments.
 */
sealed trait Segment[E, D <: Domain[E], +V] extends SegmentLike[E, D, V] { segment =>
  import Segment._

  def forwardIterable(): Iterable[Segment[E, D, V]] = new AbstractIterable[Segment[E, D, V]] {

    override def iterator: Iterator[Segment[E, D, V]] = forwardIterator()
  }

  def forwardIterator(): Iterator[Segment[E, D, V]] = new AbstractIterator[Segment[E, D, V]] {

    private var current: Segment[E, D, V] = _

    override def hasNext: Boolean = current == null || !current.isLast

    override def next(): Segment[E, D, V] = current match {
      case null =>
        current = segment
        current
      case n: WithNext[E, D, V] =>
        current = n.moveNext
        current
      case _ =>
        throw new NoSuchElementException(s"Segment $current doesn't have next segment.")
    }
  }

  def backwardIterable(): Iterable[Segment[E, D, V]] = new AbstractIterable[Segment[E, D, V]] {

    override def iterator: Iterator[Segment[E, D, V]] = backwardIterator()
  }

  def backwardIterator(): Iterator[Segment[E, D, V]] = new AbstractIterator[Segment[E, D, V]] {

    private var current: Segment[E, D, V] = _

    override def hasNext: Boolean = current == null || !current.isFirst

    override def next(): Segment[E, D, V] = current match {
      case null =>
        current = segment
        current
      case p: WithPrev[E, D, V] =>
        current = p.movePrev
        current
      case _ =>
        throw new NoSuchElementException(s"Segment $current doesn't have previous segment.")
    }
  }

  def forwardLazyList: LazyList[Segment[E, D, V]] = this match {
    case n: WithNext[E, D, V] => LazyList.cons(this, n.moveNext.forwardLazyList)
    case _                    => LazyList.cons(this, LazyList.empty)
  }

  def backwardLazyList: LazyList[Segment[E, D, V]] = this match {
    case n: WithPrev[E, D, V] => LazyList.cons(this, n.movePrev.backwardLazyList)
    case _                    => LazyList.cons(this, LazyList.empty)
  }

  def interval: Interval[E, D] = this match {
    case i: Inner[E, D, V]    => domainOps.interval(i.lowerBound, i.upperBound)
    case p: WithPrev[E, D, V] => domainOps.interval(p.lowerBound)
    case n: WithNext[E, D, V] => domainOps.interval(n.upperBound)
    case _                    => domainOps.interval.universal
  }

  def intervalRelation: IntervalRelation[E, D, V] = IntervalRelation(interval, value)

  override def toString: String = SetBuilderFormat.segment(this, (e: E) => e.toString, (v: V) => v.toString)
}

object Segment {

  type UpperBoundAscOrder[E, D <: Domain[E]] = UpperBoundOrder[E, D, AscDir]
  type UpperBoundDescOrder[E, D <: Domain[E]] = UpperBoundOrder[E, D, DescDir]

  type LowerBoundAscOrder[E, D <: Domain[E]] = LowerBoundOrder[E, D, AscDir]
  type LowerBoundDescOrder[E, D <: Domain[E]] = LowerBoundOrder[E, D, DescDir]

  implicit def upperBoundAscOrder[E, D <: Domain[E]]: UpperBoundAscOrder[E, D] = new UpperBoundOrder

  def upperBoundDescOrder[E, D <: Domain[E]]: UpperBoundDescOrder[E, D] = new UpperBoundOrder

  def lowerBoundAscOrder[E, D <: Domain[E]]: LowerBoundAscOrder[E, D] = new LowerBoundOrder

  def lowerBoundDescOrder[E, D <: Domain[E]]: LowerBoundDescOrder[E, D] = new LowerBoundOrder

  /**
   * Segment which has next segment. May be Initial or Inner.
   *
   * @see [[Segment]]
   */
  trait WithNext[E, D <: Domain[E], +V] extends Segment[E, D, V] {

    override def hasNext: Boolean = true

    override def hasUpperBound: Boolean = true

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = domainOps.boundOrd.eqv(upperBound, bound)

    def upperBound: Bound.Upper[E]

    def moveNext: WithPrev[E, D, V]

    override def forwardLazyList: LazyList[Segment[E, D, V]] = LazyList.cons(this, moveNext.forwardLazyList)

    override def interval: Interval[E, D] = this match {
      case p: WithPrev[e, d, v] => domainOps.interval(p.lowerBound, upperBound)
      case _                    => domainOps.interval(upperBound)
    }
  }

  /**
   * Segment which has previous segment. May be Terminal or Inner.
   *
   * @see [[Segment]]
   */
  trait WithPrev[E, D <: Domain[E], +V] extends Segment[E, D, V] {

    override def hasPrev: Boolean = true

    override def hasLowerBound: Boolean = true

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = domainOps.boundOrd.eqv(lowerBound, bound)

    def lowerBound: Bound.Lower[E]

    def movePrev: WithNext[E, D, V]

    override def backwardLazyList: LazyList[Segment[E, D, V]] = LazyList.cons(this, movePrev.backwardLazyList)

    override def interval: Interval[E, D] = this match {
      case n: WithNext[e, d, v] => domainOps.interval(lowerBound, n.upperBound)
      case _                    => domainOps.interval(lowerBound)
    }
  }

  /**
   * First segment of sequence. Doesn't have previous segment. May be Single or Initial.
   *
   * @see [[Segment]]
   */
  trait First[E, D <: Domain[E], +V] extends Segment[E, D, V] {

    override def isFirst: Boolean = true

    override def moveToFirst: First[E, D, V] = this

    override def interval: Interval[E, D] = this match {
      case n: WithNext[e, d, v] => domainOps.interval(n.upperBound)
      case _                    => domainOps.interval.universal
    }
  }

  /**
   * Last segment of sequence. Doesn't have next segment. May be Single or Terminal.
   *
   * @see [[Segment]]
   */
  trait Last[E, D <: Domain[E], +V] extends Segment[E, D, V] {

    override def isLast: Boolean = true

    override def moveToLast: Last[E, D, V] = this

    override def interval: Interval[E, D] = this match {
      case p: WithPrev[e, d, v] => domainOps.interval(p.lowerBound)
      case _                    => domainOps.interval.universal
    }
  }

  /**
   * The only segment in sequence (in case of empty or universal sequence) with properties:
   * <tr>- doesn't have next segment;     </tr>
   * <tr>- doesn't have previous segment. </tr>
   * <tr>                                 </tr>
   * @see [[Segment]]
   */
  trait Single[E, D <: Domain[E], +V] extends First[E, D, V] with Last[E, D, V] {

    override def isSingle: Boolean = true

    override def moveTo(bound: Bound[E]): Segment[E, D, V] = this

    override def interval: Interval[E, D] = domainOps.interval.universal

    override def toString: String =
      SetBuilderFormat.singleSegment(this, (v: V) => v.toString)
  }

  /**
   * First segment of sequence with properties:
   * <tr>- doesn't have previous segment; </tr>
   * <tr>- always has next segment.       </tr>
   * <tr>                                 </tr>
   * @see [[Segment]]
   */
  trait Initial[E, D <: Domain[E], +V] extends WithNext[E, D, V] with First[E, D, V] {

    override def isInitial: Boolean = true

    override def interval: Interval[E, D] = domainOps.interval(upperBound)

    override def toString: String =
      SetBuilderFormat.initialSegment(this, (e: E) => e.toString, (v: V) => v.toString)
  }

  /**
   * Last segment of sequence with properties:
   * <tr>- doesn't have next segment;   </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[Segment]]
   */
  trait Terminal[E, D <: Domain[E], +V] extends WithPrev[E, D, V] with Last[E, D, V] {

    override def isTerminal: Boolean = true

    override def interval: Interval[E, D] = domainOps.interval(lowerBound)

    override def toString: String =
      SetBuilderFormat.terminalSegment(this, (e: E) => e.toString, (v: V) => v.toString)
  }

  /**
   * Segment with properties:
   * <tr>- always has next segment;     </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[Segment]]
   */
  trait Inner[E, D <: Domain[E], +V] extends WithNext[E, D, V] with WithPrev[E, D, V] {

    override def isInner: Boolean = true

    override def interval: Interval[E, D] = domainOps.interval(lowerBound, upperBound)

    override def toString: String =
      SetBuilderFormat.innerSegment(this, (e: E) => e.toString, (v: V) => v.toString)
  }

  final class UpperBoundOrder[E, D <: Domain[E], Dir <: OrderDir]()(
    implicit dirValue: SingleValue[Dir]
  ) extends DirectedOrder.Abstract[Segment[E, D, Any], Dir] {

    import util.HashUtil._

    override val label: Label = OrderLabels.SegmentByUpperBound

    override def compare(x: Segment[E, D, Any], y: Segment[E, D, Any]): Int = (x, y) match {
      case (xn: WithNext[E, D, Any], yn: WithNext[E, D, Any]) => 
        x.domainOps.boundOrd.compare(xn.upperBound, yn.upperBound)
      case (_, _: WithNext[E, D, Any]) => 
        sign
      case (_: WithNext[E, D, Any], _) => 
        invertedSign
      case _ => 
        0
    }

    override def hash(x: Segment[E, D, Any]): Int = x match {
      case xn: WithNext[E, D, Any] => product1Hash(x.domainOps.boundOrd.hash(xn.upperBound))
      case _ => product1Hash(x.hashCode())
    }

    override def eqv(x: Segment[E, D, Any], y: Segment[E, D, Any]): Boolean = (x, y) match {
      case (xn: WithNext[E, D, Any], yn: WithNext[E, D, Any]) => 
        x.domainOps.boundOrd.eqv(xn.upperBound, yn.upperBound)
      case (_, _: WithNext[E, D, Any]) => 
        false
      case (_: WithNext[E, D, Any], _) => 
        false
      case _ => 
        true
    }
  }

  final class LowerBoundOrder[E, D <: Domain[E], Dir <: OrderDir]()(
    implicit dirValue: SingleValue[Dir]
  ) extends DirectedOrder.Abstract[Segment[E, D, Any], Dir] {

    import util.HashUtil._

    override def label: Label = OrderLabels.SegmentByLowerBound

    override def compare(x: Segment[E, D, Any], y: Segment[E, D, Any]): Int = (x, y) match {
      case (xp: WithPrev[E, D, Any], yp: WithPrev[E, D, Any]) => 
        x.domainOps.boundOrd.compare(xp.lowerBound, yp.lowerBound)
      case (_, _: WithPrev[E, D, Any]) => 
        invertedSign
      case (_: WithPrev[E, D, Any], _) => 
        sign
      case _ => 0
    }

    override def hash(x: Segment[E, D, Any]): Int = x match {
      case xn: WithPrev[E, D, Any] => product1Hash(x.domainOps.boundOrd.hash(xn.lowerBound))
      case _ => product1Hash(x.hashCode())
    }

    override def eqv(x: Segment[E, D, Any], y: Segment[E, D, Any]): Boolean = (x, y) match {
      case (xn: WithPrev[E, D, Any], yn: WithPrev[E, D, Any]) => 
        x.domainOps.boundOrd.eqv(xn.lowerBound, yn.lowerBound)
      case (_, _: WithPrev[E, D, Any]) => 
        false
      case (_: WithPrev[E, D, Any], _) => 
        false
      case _ => 
        true
    }
  }
}
