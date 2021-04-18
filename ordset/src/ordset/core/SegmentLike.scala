package ordset.core

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps

import scala.Specializable.{AllNumeric => spNum}
import scala.collection.{AbstractIterable, AbstractIterator}
import scala.{specialized => sp}

trait SegmentLike[@sp(spNum) E, D <: Domain[E], @sp(Boolean) V] {

  import Segment._
  
  // Inspection --------------------------------------------------------------- //
  def sequence: SegmentSeq[E, D, V]
  
  def domainOps: DomainOps[E, D] = sequence.domainOps

  def valueOps: ValueOps[V] = sequence.valueOps
  
  def value: V

  def isIncluded: Boolean

  def hasNext: Boolean = false

  def hasPrev: Boolean = false

  def hasUpperBound: Boolean = false

  def hasLowerBound: Boolean = false

  def hasUpperBound(bound: Bound.Upper[E]): Boolean = false

  def hasLowerBound(bound: Bound.Lower[E]): Boolean = false

  def hasValue(v: V): Boolean = valueOps.eqv(value, v)
  
  def isFirst: Boolean = false

  def isLast: Boolean = false

  def isInner: Boolean = false

  def isSingle: Boolean = false

  def isInitial: Boolean = false

  def isTerminal: Boolean = false

  def interval: Interval[E, D] = this match {
    case s: Inner[E, D, V]    => domainOps.interval(s.lowerBound, s.upperBound)
    case s: WithPrev[E, D, V] => domainOps.interval(s.lowerBound)
    case s: WithNext[E, D, V] => domainOps.interval(s.upperBound)
    case _                    => domainOps.interval.universal
  }

  def intervalRelation: IntervalRelation[E, D, V] = IntervalRelation(interval, value)

  override def toString: String = SetBuilderFormat.segment(self, (e: E) => e.toString, (v: V) => v.toString)
  
  // Navigation --------------------------------------------------------------- //
  def moveToFirst: Segment.First[E, D, V]

  def moveToLast: Segment.Last[E, D, V]

  def moveTo(bound: Bound[E]): Segment[E, D, V]

  def forwardIterable(): Iterable[Segment[E, D, V]] = new AbstractIterable[Segment[E, D, V]] {

    override def iterator: Iterator[Segment[E, D, V]] = forwardIterator()
  }

  def forwardIterator(): Iterator[Segment[E, D, V]] = new AbstractIterator[Segment[E, D, V]] {

    private var current: Segment[E, D, V] = _

    override def hasNext: Boolean = current == null || !current.isLast

    override def next(): Segment[E, D, V] = current match {
      case null =>
        current = self
        current
      case s: WithNext[E, D, V] =>
        current = s.moveNext
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
        current = self
        current
      case s: WithPrev[E, D, V] =>
        current = s.movePrev
        current
      case _ =>
        throw new NoSuchElementException(s"Segment $current doesn't have previous segment.")
    }
  }

  def forwardLazyList: LazyList[Segment[E, D, V]] = this match {
    case s: WithNext[E, D, V] => LazyList.cons(self, s.moveNext.forwardLazyList)
    case _                    => LazyList.cons(self, LazyList.empty)
  }

  def backwardLazyList: LazyList[Segment[E, D, V]] = this match {
    case s: WithPrev[E, D, V] => LazyList.cons(self, s.movePrev.backwardLazyList)
    case _                    => LazyList.cons(self, LazyList.empty)
  }
  
  // Transformation ----------------------------------------------------------- //
  /**
   * Returns sequence containing
   * <tr>- segment (minBound, u,,1,,) -> v,,1,,</tr>
   * <tr>- segments {i > 1: (l,,i,, u,,i,,,) -> v,,i,,} of original sequence for which l,,i,, `>` u,,1,,</tr> 
   * <tr>where</tr>
   * <tr>minBound - minimal bound of domain;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,;</tr>
   * <tr>S,,1,, - current segment;</tr>
   * {{{
   * Example 1
   *
   * original:
   *                segment
   *                   v
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * segment.takenAbove:
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.getSegment(bound).takenAbove == sequence.takenAbove(bound) 
   *   for any bound
   *
   *   2. segment.sequence == segment.takenBelow.appended(bound, segment.takenAbove) 
   *   for any bound such that segment.contains(bound) == true
   * }}}
   */
  def takenAbove: SegmentSeq[E, D, V]

  /**
   * Returns sequence containing
   * <tr>- segments {i ∈ [1, N-1]: (l,,i,, u,,i,,,) -> v,,i,,} of original sequence for which u,,i,, `<` l,,N,,</tr>
   * <tr>- segment (l,,N,,, maxBound) -> v,,N,,</tr>
   * <tr>where</tr>
   * <tr>maxBound - maximal bound of domain;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,;</tr>
   * <tr>S,,N,, - current segment;</tr>
   * {{{
   * Example 1
   *
   * original:
   *                segment
   *                   v
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * segment.takenBelow:
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.getSegment(bound).takenBelow == sequence.takenBelow(bound) 
   *   for any bound
   *
   *   2. segment.sequence == segment.takenBelow.appended(bound, segment.takenAbove) 
   *   for any bound such that segment.contains(bound) == true
   * }}}
   */
  def takenBelow: SegmentSeq[E, D, V]

  /**
   * Returns tuple of sequences: ([[takenBelow]], [[takenAbove]]).
   *
   * {{{
   * original:
   *                segment
   *                   v
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * segment.sliced._1:
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   *
   * segment.sliced._2:
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.getSegment(bound).sliced == sequence.sliced(bound) 
   *   for any bound
   *
   *   2. segment.sequence == segment.sliced._1.appended(bound, segment.sliced._2) 
   *   for any bound such that segment.contains(bound) == true
   * }}}
   */
  def sliced: (SegmentSeq[E, D, V], SegmentSeq[E, D, V])

  /**
   * original sequence (this.sequence):
   * {{{
   *
   *               current segment (this)
   *                       v
   *   X--------](------------------)[---------X
   *        A    ^         B        ^     C        - values
   *      this.lowerBound       this.upperBound
   *
   * input sequence (other):
   *
   *   X---)[-----------)[---)[-----------)[---X
   *     D        E        F        G        H     - values
   *
   * this.patched(other):
   *
   *   X--------](------)[---)[-----)[---------X
   *        A       E      F     G        C        - values
   * }}}
   */
  def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  // Protected section -------------------------------------------------------- //
  protected def self: Segment[E, D, V]
}