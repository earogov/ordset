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
  *            First         Last
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
  * @tparam D - type of domain
  * @tparam V - type of value assigned to interval
  *
  * @note Definition of segment (traits) has forward/backward symmetry: if we have `moveNext` for example there is
  *       also `movePrev` method. But its implementation may be optimized for moving forward, as it's assumed that
  *       this is the basic use case of segments.
  */
sealed trait Segment[E, +D <: Domain[E], +V] extends SegmentLike[E, D, V] {
  import Segment._

  def forwardLazyList: LazyList[Segment[E, D, V]] = this match {
    case n: WithNext[E, D, V] => LazyList.cons(this, n.moveNext.forwardLazyList)
    case _                 => LazyList.cons(this, LazyList.empty)
  }

  def backwardLazyList: LazyList[Segment[E, D, V]] = this match {
    case n: WithPrev[E, D, V] => LazyList.cons(this, n.movePrev.backwardLazyList)
    case _                 => LazyList.cons(this, LazyList.empty)
  }

  def intervalMapping: IntervalMapping[E, V] = this match {
    case i: Inner[E, D, V]    => IntervalMapping.bounded(i.lowerBound, i.upperBound, value)
    case p: WithPrev[E, D, V] => IntervalMapping.rightUnbounded(p.lowerBound, value)
    case n: WithNext[E, D, V] => IntervalMapping.leftUnbounded(n.upperBound, value)
    case _                 => IntervalMapping.unbounded(value)
  }

  override def toString: String = intervalMapping.toString
}

object Segment {

  type UpperBoundAscOrder[E] = UpperBoundOrder[E, AscDir]
  type UpperBoundDescOrder[E] = UpperBoundOrder[E, DescDir]

  type LowerBoundAscOrder[E] = LowerBoundOrder[E, AscDir]
  type LowerBoundDescOrder[E] = LowerBoundOrder[E, DescDir]

  implicit def upperBoundAscOrder[E]: UpperBoundAscOrder[E] = new UpperBoundOrder

  implicit def upperBoundDescOrder[E]: UpperBoundDescOrder[E] = new UpperBoundOrder

  def lowerBoundAscOrder[E]: LowerBoundAscOrder[E] = new LowerBoundOrder

  def lowerBoundDescOrder[E]: LowerBoundDescOrder[E] = new LowerBoundOrder

  implicit def upperBoundHash[E](implicit hash: Hash[Bound[E]]): Hash[Segment[E, Domain[E], Any]] =
    new UpperBoundHash()(hash)

  implicit def lowerBoundHash[E](implicit hash: Hash[Bound[E]]): Hash[Segment[E,  Domain[E], Any]] =
    new LowerBoundHash()(hash)

  /** Segment which has next segment. */
  trait WithNext[E, +D <: Domain[E], +V] extends Segment[E, D, V] {

    override def hasNext: Boolean = true

    def upperBound: Bound.Upper[E]

    def moveNext: WithPrev[E, D, V]

    override def forwardLazyList: LazyList[Segment[E, D, V]] = LazyList.cons(this, moveNext.forwardLazyList)

    override def intervalMapping: IntervalMapping[E, V] = this match {
      case p: WithPrev[E, D, V] => IntervalMapping.bounded(p.lowerBound, upperBound, value)
      case _                    => IntervalMapping.leftUnbounded(upperBound, value)
    }
  }

  /** Segment which has previous segment. */
  trait WithPrev[E, +D <: Domain[E], +V] extends Segment[E, D, V] {

    override def hasPrev: Boolean = true

    def lowerBound: Bound.Lower[E]

    def movePrev: WithNext[E, D, V]

    override def backwardLazyList: LazyList[Segment[E, D, V]] = LazyList.cons(this, movePrev.backwardLazyList)

    override def intervalMapping: IntervalMapping[E, V] = this match {
      case n: WithNext[E, D, V] => IntervalMapping.bounded(lowerBound, n.upperBound, value)
      case _                 => IntervalMapping.rightUnbounded(lowerBound, value)
    }
  }

  /** Segment which has both next and previous segments. */
  trait Inner[E, +D <: Domain[E], +V] extends WithNext[E, D, V] with WithPrev[E, D, V] {

    override def isInner: Boolean = true

    override def intervalMapping: IntervalMapping[E, V] = IntervalMapping.bounded(lowerBound, upperBound, value)
  }

  /**
    * First segment of sequence.
    * It may be `Single` (the only segment in sequence) or `Initial` (first segment with next).
    */
  trait First[E, +D <: Domain[E], +V] extends Segment[E, D, V] {

    override def isFirst: Boolean = true

    override def moveToFirst: First[E, D, V] = this
  }

  /**
    * Last segment of sequence.
    * It may be `Single` (the only segment in sequence) or `Terminal` (last segment with previous).
    */
  trait Last[E, +D <: Domain[E], +V] extends Segment[E, D, V] {

    override def isLast: Boolean = true

    override def moveToLast: Last[E, D, V] = this
  }

  /**
    * The only segment in sequence. Sequence in that case either empty or universal.
    */
  trait Single[E, +D <: Domain[E], +V] extends First[E, D, V] with Last[E, D, V] {

    override def isSingle: Boolean = true

    override def moveTo(bound: Bound[E]): Segment[E, D, V] = this
  }

  /**
    * First segment of sequence which has next segment. Given segment can't be `Single`.
    * Sequence in that case isn't empty or universal.
    */
  trait Initial[E, +D <: Domain[E], +V] extends WithNext[E, D, V] with First[E, D, V] {

    override def isInitial: Boolean = true
  }

  /**
    * Last segment of sequence which has previous segment. Given segment can't be `Single`.
    * Sequence in that case isn't empty or universal.
    */
  trait Terminal[E, +D <: Domain[E], +V] extends WithPrev[E, D, V] with Last[E, D, V] {

    override def isTerminal: Boolean = true
  }

  class UpperBoundOrder[E, Dir <: OrderDir](
  )(implicit override protected val dirValue: SingleValue[Dir]
  ) extends OrderWithDir.Abstract[Segment[E, Domain[E], Any], Dir] {

    override def compare(x: Segment[E, Domain[E], Any], y: Segment[E, Domain[E], Any]): Int = (x, y) match {
      case (xn: WithNext[E, _, _], yn: WithNext[E, _, _]) =>
        x.domain.boundOrd.compare(xn.upperBound, yn.upperBound)
      case (_, _: WithNext[E, _, _]) =>
        sign
      case (_: WithNext[E, _, _], _) =>
        invertedSign
      case _ =>
        0
    }
  }

  class LowerBoundOrder[E, Dir <: OrderDir](
  )(implicit override protected val dirValue: SingleValue[Dir]
  ) extends OrderWithDir.Abstract[Segment[E, Domain[E], Any], Dir] {

    override def compare(x: Segment[E, Domain[E], Any], y: Segment[E, Domain[E], Any]): Int = (x, y) match {
      case (xp: WithPrev[E, _, _], yp: WithPrev[E, _, _]) =>
        x.domain.boundOrd.compare(xp.lowerBound, yp.lowerBound)
      case (_, _: WithPrev[E, _, _]) =>
        invertedSign
      case (_: WithPrev[E, _, _], _) =>
        sign
      case _ => 0
    }
  }

  class UpperBoundHash[E](implicit hashB: Hash[Bound[E]]) extends Hash[Segment[E, Domain[E], Any]] {
    import util.Hash._

    override def hash(x: Segment[E, Domain[E], Any]): Int = x match {
      case xn: WithNext[E, _, _] => product1Hash(hashB.hash(xn.upperBound))
      case _ => singleHash
    }

    override def eqv(x: Segment[E, Domain[E], Any], y: Segment[E, Domain[E], Any]): Boolean = (x, y) match {
      case (xn: WithNext[E, _, _], yn: WithNext[E, _, _]) => hashB.eqv(xn.upperBound, yn.upperBound)
      case (_, _: WithNext[E, _, _]) => false
      case (_: WithNext[E, _, _], _) => false
      case _ => true
    }
  }

  class LowerBoundHash[E](implicit hashB: Hash[Bound[E]]) extends Hash[Segment[E, Domain[E], Any]] {
    import util.Hash._

    override def hash(x: Segment[E, Domain[E], Any]): Int = x match {
      case xn: WithPrev[E, _, _] => product1Hash(hashB.hash(xn.lowerBound))
      case _ => singleHash
    }

    override def eqv(x: Segment[E, Domain[E], Any], y: Segment[E, Domain[E], Any]): Boolean = (x, y) match {
      case (xn: WithPrev[E, _, _], yn: WithPrev[E, _, _]) => hashB.eqv(xn.lowerBound, yn.lowerBound)
      case (_, _: WithPrev[E, _, _]) => false
      case (_: WithPrev[E, _, _], _) => false
      case _ => true
    }
  }

  private val singleHash = util.Hash.product1Hash(0xE91A02B5)
}
