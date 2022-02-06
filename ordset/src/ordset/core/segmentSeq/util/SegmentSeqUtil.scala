package ordset.core.segmentSeq.util

import ordset.core.Bound
import ordset.core.domain.Domain
import ordset.core.segmentSeq.map.BoundValue
import ordset.core.segmentSeq.*

import scala.annotation.tailrec

object SegmentSeqUtil {

  /**
   * Returns upper bounds of all segments from current (including or excluding) to last (including).
   * @return iterable of upper bounds.
   */
  def getUpperBoundsIterableFromSegment[E, D[X] <: Domain[X], V](
    segment: Segment[E, D, V],
    including: Boolean
  ): Iterable[Bound.Upper[E]] = {
    import scala.language.unsafeNulls
    val startSegment =
      if (including) segment
      else segment match {
        case s: Segment.WithNext[E, D, V] => s.moveNext
        case _ => null
      }
    if (startSegment == null) Nil
    else startSegment.forwardIterable.map {
      case s: Segment.WithNext[E, D, V] => s.upper
      case _ => null
    }.takeWhile(_ != null)
  }

  /**
   * Returns upper bounds of all segments from first (including) to current (including or excluding).
   * @return iterable of upper bounds.
   */
  def getUpperBoundsIterableToSegment[E, D[X] <: Domain[X], V](
    segment: Segment[E, D, V],
    including: Boolean
  ): Iterable[Bound.Upper[E]] = {
    import scala.language.unsafeNulls
    if (including)
      segment.moveToFirst.forwardIterable.map { s =>
        if (segment.domainOps.segments.upperOrd.gt(s, segment)) null
        else s match {
          case s: Segment.WithNext[E, D, V] => s.upper
          case _ => null
        }
      }.takeWhile(_ != null)
    else
      segment.moveToFirst.forwardIterable.map { s =>
        if (segment.domainOps.segments.upperOrd.gteqv(s, segment)) null
        else s match {
          case s: Segment.WithNext[E, D, V] => s.upper
          case _ => null
        }
      }.takeWhile(_ != null)
  }

  /**
   * Returns upper bounds of all segments from current (including or excluding) to last (including).
   * @return list of upper bounds with its size.
   */
  def getUpperBoundsListFromSegment[E, D[X] <: Domain[X], V](
    segment: Segment[E, D, V],
    including: Boolean
  ): (List[Bound.Upper[E]], Int) = {
    var size = 0
    var list: List[Bound.Upper[E]] = Nil

    val ord = segment.domainOps.segments.upperOrd
    val loopCondition =
      if (including) (s: Segment[E, D, V]) => ord.gteqv(s, segment)
      else (s: Segment[E, D, V]) => ord.gt(s, segment)

    @tailrec
    def loop(s: Segment[E, D, V]): Unit = if (loopCondition(s)) {
      s match {
        case s: Segment.WithNext[E, D, V] =>
          size = size + 1
          list = list.prepended(s.upper)
        case _ => // nothing to do
      }
      s match {
        case s: Segment.WithPrev[E, D, V] => loop(s.movePrev)
        case _ => // stop
      }
    }

    loop(segment.moveToLast)
    (list, size)
  }

  /**
   * Returns upper bounds of all segments from first (including) to current (including or excluding).
   * @return list of upper bounds with its size.
   */
  def getUpperBoundsListToSegment[E, D[X] <: Domain[X], V](
    segment: Segment[E, D, V],
    including: Boolean
  ): (List[Bound.Upper[E]], Int) = {
    var size = 0
    var list = List.empty[Bound.Upper[E]]

    @tailrec
    def loop(s: Segment[E, D, V]): Unit = {
      s match {
        case s: Segment.WithNext[E, D, V] =>
          size = size + 1
          list = list.prepended(s.upper)
        case _ => // nothing to do
      }
      s match {
        case s: Segment.WithPrev[E, D, V] => loop(s.movePrev)
        case _ => // stop
      }
    }

    val startSegment =
      if (including) segment
      else segment match {
        case s: Segment.WithPrev[E, D, V] => s.movePrev
        case _ => null
      }
    if (startSegment != null) loop(startSegment)
    (list, size)
  }

  /**
   * Returns iterable of (bound, value) tuples for given segment sequence.
   *
   * {{{
   *     seq:
   *
   *       segment_0       segment_1     segment_i    segment_(n-1)
   *     X------------)[------------](-------------](-----------X
   *
   *                               V V V
   *     output:
   *
   *         value_0       value_1       value_i        value_(n-1)
   *      ------------) ------------] -------------] -----------X
   *            bound_0       bound_1         bound_i       ExtendedBound.AboveAll
   *
   *     n = seq.size  - number of segments
   * }}}
   *
   * Output iterable has at least one element. Last element has bound == [[ExtendedBound.AboveAll]].
   */
  def getBoundValueIterableForSeq[E, D[X] <: Domain[X], V](
    seq: SegmentSeq[E, D, V]
  ): Iterable[BoundValue[E, V]] =
    seq.firstSegment.forwardIterable.map(s => (s.upper, s.value))

  /**
   * Returns tuple of segments of sequence `seq`:
   * 
   * tuple._1 - segment that contains lower bound of `segment` or first segment of `seq` if `segment` is first.
   * 
   * tuple._2 - segment that contains upper bound of `segment` or last segment of `seq` if `segment` is last.
   * 
   * {{{
   *   segment:
   *           
   *               [--------------]
   *             lower          upper
   *   seq:
   *   
   *       A       B      C       D         E 
   *   X------](------)[----](---------](-------X
   *   
   *   output:
   *               B              D
   *           (------)      (---------]
   *           tuple._1        tuple._2
   * }}}
   */
  def getBoundSegments[E, D[X] <: Domain[X], V, S](
    segment: Segment[E, D, ?], 
    seq: SegmentSeqT[E, D, V, S]
  ): (SegmentT[E, D, V, S], SegmentT[E, D, V, S]) =
    segment match {
      case s: Segment.Inner[E, D, ?]    => (seq.getSegmentForBound(s.lower), seq.getSegmentForBound(s.upper))
      case s: Segment.WithNext[E, D, ?] => (seq.firstSegment, seq.getSegmentForBound(s.upper))
      case s: Segment.WithPrev[E, D, ?] => (seq.getSegmentForBound(s.lower), seq.lastSegment)
      case _                            => (seq.firstSegment, seq.lastSegment)
    }

  /**
   * Returns segment that contains lower bound of `segment` or first segment of `seq` if `segment` is first.
   *
   * {{{
   *   segment:
   *
   *               [--------------]
   *             lower
   *   seq:
   *
   *       A       B      C       D         E
   *   X------](------)[----](---------](-------X
   *
   *   output:
   *               B              
   *           (------)
   * }}}
   */
  def getLowerBoundSegment[E, D[X] <: Domain[X], V, S](
    segment: Segment[E, D, ?],
    seq: SegmentSeqT[E, D, V, S]
  ): SegmentT[E, D, V, S] =
    segment match {
      case s: Segment.WithPrev[E, D, ?] => seq.getSegmentForBound(s.lower)
      case _ => seq.firstSegment
    }

  /**
   * Returns segment that contains upper bound of `segment` or last segment of `seq` if `segment` is last.
   *
   * {{{
   *   segment:
   *
   *               [--------------]
   *                            upper
   *   seq:
   *
   *       A       B      C       D         E 
   *   X------](------)[----](---------](-------X
   *
   *   output:
   *                              D
   *                         (---------]
   * }}}
   */
  def getUpperBoundSegment[E, D[X] <: Domain[X], V, S](
    segment: Segment[E, D, ?],
    seq: SegmentSeqT[E, D, V, S]
  ): SegmentT[E, D, V, S] =
    segment match {
      case s: Segment.WithNext[E, D, ?] => seq.getSegmentForBound(s.upper)
      case _ => seq.lastSegment
    }
}
