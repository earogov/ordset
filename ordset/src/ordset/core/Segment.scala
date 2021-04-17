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
 * <h1>Notes</h1>
 * 
 * 1. In all ordering relations of bounds (like bound1 `>` bound2 etc.) we assume:
 * <tr>- upper bound of last segment has maximal value (equivalent to plus infinity);   </tr>
 * <tr>- lower bound of first segment has minimal value (equivalent to minus infinity). </tr>
 * <tr>
 * These properties MUST be provided by implementations of [[DomainOps.segmentUpperOrd]] and
 * [[DomainOps.segmentLowerOrd]].
 * </tr>
 * <tr></tr>
 *
 * 2. Definition of segment (traits) has forward/backward symmetry: for example if we have `moveNext` there is
 * also `movePrev` method. But its implementation may be optimized to move forward, as it's assumed this is
 * the basic use case of segments.
 * <tr></tr>
 * 
 * @tparam E type of element in ordered set
 * @tparam D type of domain
 * @tparam V type of value assigned to interval
 */
sealed trait Segment[E, D <: Domain[E], V] extends SegmentLike[E, D, V] { segment =>
  import Segment._

  // Inspection --------------------------------------------------------------- //
  def interval: Interval[E, D] = this match {
    case s: Inner[E, D, V]    => domainOps.interval(s.lowerBound, s.upperBound)
    case s: WithPrev[E, D, V] => domainOps.interval(s.lowerBound)
    case s: WithNext[E, D, V] => domainOps.interval(s.upperBound)
    case _                    => domainOps.interval.universal
  }

  def intervalRelation: IntervalRelation[E, D, V] = IntervalRelation(interval, value)
  
  override def toString: String = SetBuilderFormat.segment(this, (e: E) => e.toString, (v: V) => v.toString)

  // Navigation --------------------------------------------------------------- //
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
        current = segment
        current
      case s: WithPrev[E, D, V] =>
        current = s.movePrev
        current
      case _ =>
        throw new NoSuchElementException(s"Segment $current doesn't have previous segment.")
    }
  }

  def forwardLazyList: LazyList[Segment[E, D, V]] = this match {
    case s: WithNext[E, D, V] => LazyList.cons(this, s.moveNext.forwardLazyList)
    case _                    => LazyList.cons(this, LazyList.empty)
  }

  def backwardLazyList: LazyList[Segment[E, D, V]] = this match {
    case s: WithPrev[E, D, V] => LazyList.cons(this, s.movePrev.backwardLazyList)
    case _                    => LazyList.cons(this, LazyList.empty)
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
  def takenAbove: SegmentSeq[E, D, V] = ???

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
  def takenBelow: SegmentSeq[E, D, V] = ???

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
   * segement.sliced._1:
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   *
   * segement.sliced._2:
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
  def sliced: (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = (takenBelow, takenAbove)
  
  /**
   * original sequence (this.sequence):
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
  // TODO implement `SegmentSeq.prepended` and then `Segment.patched`.
  def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???
//    this match {
//    case s: Inner[E, D, V] =>
//      val nextLowerBound = s.upperBound.flip
//      val (originalLeft, originalTail) = sequence.sliced(s.lowerBound)
//      val originalRight = originalTail.takenAbove(nextLowerBound)
//      val patch = other.takenBelow(nextLowerBound)
//      originalLeft.appended(patch).appended(originalRight)
//    case s: WithNext[E, D, V] =>
//      val nextLowerBound = s.upperBound.flip
//      val originalRight = sequence.takenAbove(nextLowerBound)
//      val patch = other.takenBelow(nextLowerBound)
//      patch.appended(originalRight)
//    case s: WithPrev[E, D, V] => 
//      val originalLeft = sequence.takenBelow(s.lowerBound)
//      originalLeft.appended(other)
//    case _ =>
//      other
//  }
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
  trait WithNext[E, D <: Domain[E], V] extends Segment[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def hasNext: Boolean = true

    override def hasUpperBound: Boolean = true

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = domainOps.boundOrd.eqv(upperBound, bound)

    def upperBound: Bound.Upper[E]

    override def interval: Interval[E, D] = this match {
      case s: WithPrev[e, d, v] => domainOps.interval(s.lowerBound, upperBound)
      case _                    => domainOps.interval(upperBound)
    }
    
    // Navigation --------------------------------------------------------------- //
    def moveNext: WithPrev[E, D, V]

    override def forwardLazyList: LazyList[Segment[E, D, V]] = LazyList.cons(this, moveNext.forwardLazyList)

    // Transformation ----------------------------------------------------------- //
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???
  }

  /**
   * Segment which has previous segment. May be Terminal or Inner.
   *
   * @see [[Segment]]
   */
  trait WithPrev[E, D <: Domain[E], V] extends Segment[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def hasPrev: Boolean = true

    override def hasLowerBound: Boolean = true

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = domainOps.boundOrd.eqv(lowerBound, bound)

    def lowerBound: Bound.Lower[E]

    override def interval: Interval[E, D] = this match {
      case s: WithNext[e, d, v] => domainOps.interval(lowerBound, s.upperBound)
      case _                    => domainOps.interval(lowerBound)
    }
    
    // Navigation --------------------------------------------------------------- //
    def movePrev: WithNext[E, D, V]

    override def backwardLazyList: LazyList[Segment[E, D, V]] = LazyList.cons(this, movePrev.backwardLazyList)

    // Transformation ----------------------------------------------------------- //
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???
  }

  /**
   * First segment of sequence. Doesn't have previous segment. May be Single or Initial.
   *
   * @see [[Segment]]
   */
  trait First[E, D <: Domain[E], V] extends Segment[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def isFirst: Boolean = true

    override def moveToFirst: First[E, D, V] = this

    override def interval: Interval[E, D] = this match {
      case s: WithNext[e, d, v] => domainOps.interval(s.upperBound)
      case _                    => domainOps.interval.universal
    }

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: SegmentSeq[E, D, V] = sequence
    
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???
  }

  /**
   * Last segment of sequence. Doesn't have next segment. May be Single or Terminal.
   *
   * @see [[Segment]]
   */
  trait Last[E, D <: Domain[E], V] extends Segment[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def isLast: Boolean = true

    override def moveToLast: Last[E, D, V] = this

    override def interval: Interval[E, D] = this match {
      case s: WithPrev[e, d, v] => domainOps.interval(s.lowerBound)
      case _                    => domainOps.interval.universal
    }

    // Transformation ----------------------------------------------------------- //
    override def takenBelow: SegmentSeq[E, D, V] = sequence
    
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???
  }

  /**
   * The only segment in sequence (in case of empty or universal sequence) with properties:
   * <tr>- doesn't have next segment;     </tr>
   * <tr>- doesn't have previous segment. </tr>
   * <tr>                                 </tr>
   * @see [[Segment]]
   */
  trait Single[E, D <: Domain[E], V] extends First[E, D, V] with Last[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def isSingle: Boolean = true

    override def moveTo(bound: Bound[E]): Single[E, D, V] = this

    override def interval: Interval[E, D] = domainOps.interval.universal

    override def toString: String =
      SetBuilderFormat.singleSegment(this, (v: V) => v.toString)

    // Transformation ----------------------------------------------------------- //
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = other
  }

  /**
   * Initial segment of sequence with properties:
   * <tr>- doesn't have previous segment; </tr>
   * <tr>- always has next segment.       </tr>
   * <tr>                                 </tr>
   * @see [[Segment]]
   */
  trait Initial[E, D <: Domain[E], V] extends WithNext[E, D, V] with First[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def isInitial: Boolean = true

    override def interval: Interval[E, D] = domainOps.interval(upperBound)

    override def toString: String =
      SetBuilderFormat.initialSegment(this, (e: E) => e.toString, (v: V) => v.toString)

    // Transformation ----------------------------------------------------------- //
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???
  }

  /**
   * Terminal segment of sequence with properties:
   * <tr>- doesn't have next segment;   </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[Segment]]
   */
  trait Terminal[E, D <: Domain[E], V] extends WithPrev[E, D, V] with Last[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def isTerminal: Boolean = true

    override def interval: Interval[E, D] = domainOps.interval(lowerBound)

    override def toString: String =
      SetBuilderFormat.terminalSegment(this, (e: E) => e.toString, (v: V) => v.toString)

    // Transformation ----------------------------------------------------------- //
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???
  }

  /**
   * Segment with properties:
   * <tr>- always has next segment;     </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[Segment]]
   */
  trait Inner[E, D <: Domain[E], V] extends WithNext[E, D, V] with WithPrev[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def isInner: Boolean = true

    override def interval: Interval[E, D] = domainOps.interval(lowerBound, upperBound)

    override def toString: String =
      SetBuilderFormat.innerSegment(this, (e: E) => e.toString, (v: V) => v.toString)

    // Transformation ----------------------------------------------------------- //
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???
  }

  final class UpperBoundOrder[E, D <: Domain[E], Dir <: OrderDir]()(
    implicit dirValue: SingleValue[Dir]
  ) extends DirectedOrder.Abstract[Segment[E, D, ?], Dir] {

    import util.HashUtil._

    override val label: Label = OrderLabels.SegmentByUpperBound

    override def compare(x: Segment[E, D, ?], y: Segment[E, D, ?]): Int = (x, y) match {
      case (x: WithNext[E, D, ?], y: WithNext[E, D, ?]) => 
        x.domainOps.boundOrd.compare(x.upperBound, y.upperBound)
      case (_, _: WithNext[E, D, ?]) => 
        sign
      case (_: WithNext[E, D, ?], _) => 
        invertedSign
      case _ => 
        0
    }

    override def hash(x: Segment[E, D, ?]): Int = x match {
      case x: WithNext[E, D, ?] => product1Hash(x.domainOps.boundOrd.hash(x.upperBound))
      case _ => product1Hash(x.hashCode())
    }

    override def eqv(x: Segment[E, D, ?], y: Segment[E, D, ?]): Boolean = (x, y) match {
      case (x: WithNext[E, D, ?], y: WithNext[E, D, ?]) => 
        x.domainOps.boundOrd.eqv(x.upperBound, y.upperBound)
      case (_, _: WithNext[E, D, ?]) => 
        false
      case (_: WithNext[E, D, ?], _) => 
        false
      case _ => 
        true
    }
  }

  final class LowerBoundOrder[E, D <: Domain[E], Dir <: OrderDir]()(
    implicit dirValue: SingleValue[Dir]
  ) extends DirectedOrder.Abstract[Segment[E, D, ?], Dir] {

    import util.HashUtil._

    override def label: Label = OrderLabels.SegmentByLowerBound

    override def compare(x: Segment[E, D, ?], y: Segment[E, D, ?]): Int = (x, y) match {
      case (x: WithPrev[E, D, ?], y: WithPrev[E, D, ?]) => 
        x.domainOps.boundOrd.compare(x.lowerBound, y.lowerBound)
      case (_, _: WithPrev[E, D, ?]) => 
        invertedSign
      case (_: WithPrev[E, D, ?], _) => 
        sign
      case _ => 0
    }

    override def hash(x: Segment[E, D, ?]): Int = x match {
      case x: WithPrev[E, D, ?] => product1Hash(x.domainOps.boundOrd.hash(x.lowerBound))
      case _ => product1Hash(x.hashCode())
    }

    override def eqv(x: Segment[E, D, ?], y: Segment[E, D, ?]): Boolean = (x, y) match {
      case (x: WithPrev[E, D, ?], y: WithPrev[E, D, ?]) => 
        x.domainOps.boundOrd.eqv(x.lowerBound, y.lowerBound)
      case (_, _: WithPrev[E, D, ?]) => 
        false
      case (_: WithPrev[E, D, ?], _) => 
        false
      case _ => 
        true
    }
  }
}
