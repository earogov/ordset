package ordset

import ordset.util.SingleValue

/**
  * Segment is equivalent to interval with some value assigned to it.
  * The main feature of segments is that they cover ordered universal set without gaps and overlapping.
  * So we can move from given segment to the next, previous, first or last.
  * Segments have next hierarchy (subclass -> superclass):
  *
  *                    Single
  *                 ↙        ↘
  *            First           Last
  *          ↗      ↘      ↙      ↘
  *     Initial      Segment       Terminal
  *          ↘      ↗     ↖       ↗
  *           WithNext      WithPrev
  *                 ↖      ↗
  *                   Inner
  *
  * For details see description of corresponding traits.
  *
  * @tparam E - type of element in ordered set
  * @tparam V - type of value assigned to interval
  *
  * @note Definition of segment (traits) has forward/backward symmetry: if we have `moveNext` for example there is
  *       also `movePrev` method. But its implementation may be optimized for moving forward, as it's assumed that
  *       this is the basic use case of segments.
  */
sealed trait Segment[E, +V] extends SegmentLike[E, V] {
  import Segment._

  def forwardLazyList: LazyList[Segment[E, V]] = this match {
    case n: WithNext[E, V] => LazyList.cons(this, n.moveNext.forwardLazyList)
    case _                 => LazyList.empty
  }

  def backwardLazyList: LazyList[Segment[E, V]] = this match {
    case n: WithPrev[E, V] => LazyList.cons(this, n.movePrev.backwardLazyList)
    case _                 => LazyList.empty
  }

  def intervalMapping: IntervalMapping[E, V] = this match {
    case i: Inner[E, V]    => IntervalMapping.bounded(i.lowerBound, i.upperBound, value)
    case p: WithPrev[E, V] => IntervalMapping.rightUnbounded(p.lowerBound, value)
    case n: WithNext[E, V] => IntervalMapping.leftUnbounded(n.upperBound, value)
    case _                 => IntervalMapping.unbounded(value)
  }

  override def toString: String = intervalMapping.toString
}

object Segment {

  type UpperBoundAscOrder[E] = UpperBoundOrder[E, AscDir]
  type UpperBoundDescOrder[E] = UpperBoundOrder[E, DescDir]

  type LowerBoundAscOrder[E] = LowerBoundOrder[E, AscDir]
  type LowerBoundDescOrder[E] = LowerBoundOrder[E, DescDir]

  implicit def upperBoundAscOrder[E](implicit boundOrd: AscOrder[Bound[E]]): UpperBoundAscOrder[E] =
    new UpperBoundOrder(boundOrd)

  implicit def upperBoundDescOrder[E](implicit boundOrd: DescOrder[Bound[E]]): UpperBoundDescOrder[E]  =
    new UpperBoundOrder(boundOrd)

  def lowerBoundAscOrder[E](implicit boundOrd: AscOrder[Bound[E]]): LowerBoundAscOrder[E] =
    new LowerBoundOrder(boundOrd)

  def lowerBoundDescOrder[E](implicit boundOrd: DescOrder[Bound[E]]): LowerBoundDescOrder[E] =
    new LowerBoundOrder(boundOrd)

  implicit def upperBoundHash[E, V](implicit hash: Hash[Bound[E]]): Hash[Segment[E, V]] =
    new UpperBoundHash()(hash)

  implicit def lowerBoundHash[E, V](implicit hash: Hash[Bound[E]]): Hash[Segment[E, V]] =
    new LowerBoundHash()(hash)

  /** Segment which has next segment. */
  trait WithNext[E, +V] extends Segment[E, V] {

    override def hasNext: Boolean = true

    def upperBound: Bound.Upper[E]

    def moveNext: WithPrev[E, V]

    override def forwardLazyList: LazyList[Segment[E, V]] = LazyList.cons(this, moveNext.forwardLazyList)

    override def intervalMapping: IntervalMapping[E, V] = this match {
      case p: WithPrev[E, V] => IntervalMapping.bounded(p.lowerBound, upperBound, value)
      case _                 => IntervalMapping.leftUnbounded(upperBound, value)
    }
  }

  /** Segment which has previous segment. */
  trait WithPrev[E, +V] extends Segment[E, V] {

    override def hasPrev: Boolean = true

    def lowerBound: Bound.Lower[E]

    def movePrev: WithNext[E, V]

    override def backwardLazyList: LazyList[Segment[E, V]] = LazyList.cons(this, movePrev.backwardLazyList)

    override def intervalMapping: IntervalMapping[E, V] = this match {
      case n: WithNext[E, V] => IntervalMapping.bounded(lowerBound, n.upperBound, value)
      case _                 => IntervalMapping.rightUnbounded(lowerBound, value)
    }
  }

  /** Segment which has both next and previous segments. */
  trait Inner[E, +V] extends WithNext[E, V] with WithPrev[E, V] {

    override def isInner: Boolean = true

    override def intervalMapping: IntervalMapping[E, V] = IntervalMapping.bounded(lowerBound, upperBound, value)
  }

  /**
    * First segment of sequence.
    * It may be `Single` (the only segment in sequence) or `Initial` (first segment with next).
    */
  trait First[E, +V] extends Segment[E, V] {

    override def isFirst: Boolean = true

    override def moveToFirst: First[E, V] = this
  }

  /**
    * Last segment of sequence.
    * It may be `Single` (the only segment in sequence) or `Terminal` (last segment with previous).
    */
  trait Last[E, +V] extends Segment[E, V] {

    override def isLast: Boolean = true

    override def moveToLast: Last[E, V] = this
  }

  /**
    * The only segment in sequence. Sequence in that case either empty or universal.
    */
  trait Single[E, +V] extends First[E, V] with Last[E, V] {

    override def isSingle: Boolean = true

    override def moveTo(bound: Bound[E]): Segment[E, V] = this
  }

  /**
    * First segment of sequence which has next segment. Given segment can't be `Single`.
    * Sequence in that case isn't empty or universal.
    */
  trait Initial[E, +V] extends WithNext[E, V] with First[E, V] {

    override def isInitial: Boolean = true
  }

  /**
    * Last segment of sequence which has previous segment. Given segment can't be `Single`.
    * Sequence in that case isn't empty or universal.
    */
  trait Terminal[E, +V] extends WithPrev[E, V] with Last[E, V] {

    override def isTerminal: Boolean = true
  }

  class UpperBoundOrder[E, Dir <: OrderDir](
      val boundOrd: OrderWithDir[Bound[E], Dir]
  )(
      implicit override protected val dirValue: SingleValue[Dir]
  ) extends OrderWithDir.Abstract[Segment[E, Nothing], Dir] {

    override def compare(x: Segment[E, Nothing], y: Segment[E, Nothing]): Int = (x, y) match {
      case (xn: WithNext[E, Nothing], yn: WithNext[E, Nothing]) => boundOrd.compare(xn.upperBound, yn.upperBound)
      case (_, _: WithNext[E, Nothing]) => sign
      case (_: WithNext[E, Nothing], _) => invertedSign
      case _ => 0
    }
  }

  class LowerBoundOrder[E, Dir <: OrderDir](
      val boundOrd: OrderWithDir[Bound[E], Dir]
  )(
      implicit override protected val dirValue: SingleValue[Dir]
  ) extends OrderWithDir.Abstract[Segment[E, Nothing], Dir] {

    override def compare(x: Segment[E, Nothing], y: Segment[E, Nothing]): Int = (x, y) match {
      case (xp: WithPrev[E, Nothing], yp: WithPrev[E, Nothing]) => boundOrd.compare(xp.lowerBound, yp.lowerBound)
      case (_, _: WithPrev[E, Nothing]) => invertedSign
      case (_: WithPrev[E, Nothing], _) => sign
      case _ => 0
    }
  }

  class UpperBoundHash[E, V](implicit hashB: Hash[Bound[E]]) extends Hash[Segment[E, V]] {
    import util.Hash._

    override def hash(x: Segment[E, V]): Int = x match {
      case xn: WithNext[E, V] => product1Hash(hashB.hash(xn.upperBound))
      case _ => singleHash
    }

    override def eqv(x: Segment[E, V], y: Segment[E, V]): Boolean = (x, y) match {
      case (xn: WithNext[E, V], yn: WithNext[E, V]) => hashB.eqv(xn.upperBound, yn.upperBound)
      case (_, _: WithNext[E, V]) => false
      case (_: WithNext[E, V], _) => false
      case _ => true
    }
  }

  class LowerBoundHash[E, V](implicit hashB: Hash[Bound[E]]) extends Hash[Segment[E, V]] {
    import util.Hash._

    override def hash(x: Segment[E, V]): Int = x match {
      case xn: WithPrev[E, V] => product1Hash(hashB.hash(xn.lowerBound))
      case _ => singleHash
    }

    override def eqv(x: Segment[E, V], y: Segment[E, V]): Boolean = (x, y) match {
      case (xn: WithPrev[E, V], yn: WithPrev[E, V]) => hashB.eqv(xn.lowerBound, yn.lowerBound)
      case (_, _: WithPrev[E, V]) => false
      case (_: WithPrev[E, V], _) => false
      case _ => true
    }
  }

  private val singleHash = util.Hash.product1Hash(0xE91A02B5)
}
