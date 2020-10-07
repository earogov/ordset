package ordset

import ordset.domain.{AscDir, DescDir, DirectedOrder, Domain, OrderDir}
import ordset.util.label.Label
import ordset.util.types.SingleValue

/**
  * Segment is equivalent to interval with some value assigned to it.
  * The main feature of segments is that they cover ordered universal set without gaps and overlapping.
  * So we can move from given segment to the next, previous, first or last.
  * Segments have next hierarchy (subclass -> superclass):
  * {{{
 *
  *                   Single
  *                 ↙        ↘
  *            First         Last
  *          ↗      ↘      ↙      ↘
  *     Initial      Segment       Terminal
  *          ↘      ↗     ↖       ↗
  *           WithNext      WithPrev
  *                 ↖      ↗
  *                  Inner
  * }}}
  * For details see description of corresponding traits.
  *
  * @tparam E type of element in ordered set
  * @tparam D type of domain
  * @tparam V type of value assigned to interval
  *
  * @note Definition of segment (traits) has forward/backward symmetry: for example if we have `moveNext` there is
  *       also `movePrev` method. But its implementation may be optimized to move forward, as it's assumed this the
  *       the basic use case of segments.
  */
sealed trait Segment[E, D <: Domain[E], +V] extends SegmentLike[E, D, V] {
  import Segment._

  def forwardLazyList: LazyList[Segment[E, D, V]] = this match {
    case n: WithNext[E, D, V] => LazyList.cons(this, n.moveNext.forwardLazyList)
    case _                    => LazyList.cons(this, LazyList.empty)
  }

  def backwardLazyList: LazyList[Segment[E, D, V]] = this match {
    case n: WithPrev[E, D, V] => LazyList.cons(this, n.movePrev.backwardLazyList)
    case _                    => LazyList.cons(this, LazyList.empty)
  }

  def intervalMapping: IntervalMapping[E, D, V] = this match {
    case i: Inner[E, D, V]    => IntervalMapping(domainOps.interval(i.lowerBound, i.upperBound), value)
    case p: WithPrev[E, D, V] => IntervalMapping(domainOps.interval(p.lowerBound), value)
    case n: WithNext[E, D, V] => IntervalMapping(domainOps.interval(n.upperBound), value)
    case _                    => IntervalMapping(domainOps.interval.universal, value)
  }

  override def toString: String = intervalMapping.toString
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

  /** Segment which has next segment. */
  trait WithNext[E, D <: Domain[E], +V] extends Segment[E, D, V] {

    override def hasNext: Boolean = true

    def upperBound: Bound.Upper[E]

    def moveNext: WithPrev[E, D, V]

    override def forwardLazyList: LazyList[Segment[E, D, V]] = LazyList.cons(this, moveNext.forwardLazyList)

    override def intervalMapping: IntervalMapping[E, D, V] = this match {
      case p: WithPrev[E, D, V] => IntervalMapping(domainOps.interval(p.lowerBound, upperBound), value)
      case _                    => IntervalMapping(domainOps.interval(upperBound), value)
    }
  }

  /** Segment which has previous segment. */
  trait WithPrev[E, D <: Domain[E], +V] extends Segment[E, D, V] {

    override def hasPrev: Boolean = true

    def lowerBound: Bound.Lower[E]

    def movePrev: WithNext[E, D, V]

    override def backwardLazyList: LazyList[Segment[E, D, V]] = LazyList.cons(this, movePrev.backwardLazyList)

    override def intervalMapping: IntervalMapping[E, D, V] = this match {
      case n: WithNext[E, D, V] => IntervalMapping(domainOps.interval(lowerBound, n.upperBound), value)
      case _                    => IntervalMapping(domainOps.interval(lowerBound), value)
    }
  }

  /** Segment which has both next and previous segments. */
  trait Inner[E, D <: Domain[E], +V] extends WithNext[E, D, V] with WithPrev[E, D, V] {

    override def isInner: Boolean = true

    override def intervalMapping: IntervalMapping[E, D, V] =
      IntervalMapping(domainOps.interval(lowerBound, upperBound), value)
  }

  /**
    * First segment of sequence.
    * It may be `Single` (the only segment in sequence) or `Initial` (first segment with next).
    */
  trait First[E, D <: Domain[E], +V] extends Segment[E, D, V] {

    override def isFirst: Boolean = true

    override def moveToFirst: First[E, D, V] = this
  }

  /**
    * Last segment of sequence.
    * It may be `Single` (the only segment in sequence) or `Terminal` (last segment with previous).
    */
  trait Last[E, D <: Domain[E], +V] extends Segment[E, D, V] {

    override def isLast: Boolean = true

    override def moveToLast: Last[E, D, V] = this
  }

  /**
    * The only segment in sequence. Sequence in that case either empty or universal.
    */
  trait Single[E, D <: Domain[E], +V] extends First[E, D, V] with Last[E, D, V] {

    override def isSingle: Boolean = true

    override def moveTo(bound: Bound[E]): Segment[E, D, V] = this
  }

  /**
    * First segment of sequence which has next segment. Given segment can't be `Single`.
    * Sequence in that case isn't empty or universal.
    */
  trait Initial[E, D <: Domain[E], +V] extends WithNext[E, D, V] with First[E, D, V] {

    override def isInitial: Boolean = true
  }

  /**
    * Last segment of sequence which has previous segment. Given segment can't be `Single`.
    * Sequence in that case isn't empty or universal.
    */
  trait Terminal[E, D <: Domain[E], +V] extends WithPrev[E, D, V] with Last[E, D, V] {

    override def isTerminal: Boolean = true
  }

  final class UpperBoundOrder[E, D <: Domain[E], Dir <: OrderDir]()(
    implicit dirValue: SingleValue[Dir]
  ) extends DirectedOrder.Abstract[Segment[E, D, Any], Dir] {

    import util.Hash._

    override val label: Label = OrderLabels.SegmentByUpperBound

    override def compare(x: Segment[E, D, Any], y: Segment[E, D, Any]): Int = (x, y) match {
      case (xn: WithNext[E, _, _], yn: WithNext[E, _, _]) => x.domainOps.boundOrd.compare(xn.upperBound, yn.upperBound)
      case (_, _: WithNext[E, _, _]) => sign
      case (_: WithNext[E, _, _], _) => invertedSign
      case _ => 0
    }

    override def hash(x: Segment[E, D, Any]): Int = x match {
      case xn: WithNext[E, _, _] => product1Hash(x.domainOps.boundOrd.hash(xn.upperBound))
      case _ => product1Hash(x.hashCode())
    }

    override def eqv(x: Segment[E, D, Any], y: Segment[E, D, Any]): Boolean = (x, y) match {
      case (xn: WithNext[E, _, _], yn: WithNext[E, _, _]) => x.domainOps.boundOrd.eqv(xn.upperBound, yn.upperBound)
      case (_, _: WithNext[E, _, _]) => false
      case (_: WithNext[E, _, _], _) => false
      case _ => true
    }
  }

  final class LowerBoundOrder[E, D <: Domain[E], Dir <: OrderDir]()(
    implicit dirValue: SingleValue[Dir]
  ) extends DirectedOrder.Abstract[Segment[E, D, Any], Dir] {

    import util.Hash._

    override def label: Label = OrderLabels.SegmentByLowerBound

    override def compare(x: Segment[E, D, Any], y: Segment[E, D, Any]): Int = (x, y) match {
      case (xp: WithPrev[E, _, _], yp: WithPrev[E, _, _]) => x.domainOps.boundOrd.compare(xp.lowerBound, yp.lowerBound)
      case (_, _: WithPrev[E, _, _]) => sign
      case (_: WithPrev[E, _, _], _) => invertedSign
      case _ => 0
    }

    override def hash(x: Segment[E, D, Any]): Int = x match {
      case xn: WithPrev[E, _, _] => product1Hash(x.domainOps.boundOrd.hash(xn.lowerBound))
      case _ => product1Hash(x.hashCode())
    }

    override def eqv(x: Segment[E, D, Any], y: Segment[E, D, Any]): Boolean = (x, y) match {
      case (xn: WithPrev[E, _, _], yn: WithPrev[E, _, _]) => x.domainOps.boundOrd.eqv(xn.lowerBound, yn.lowerBound)
      case (_, _: WithPrev[E, _, _]) => false
      case (_: WithPrev[E, _, _], _) => false
      case _ => true
    }
  }
}
