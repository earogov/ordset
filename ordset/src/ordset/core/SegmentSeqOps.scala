package ordset.core

import ordset.core.domain.Domain

import scala.annotation.tailrec

object SegmentSeqOps {

  /**
   * Returns upper bounds of subsequent segments from the given segment (inclusive).
   * @return iterable of upper bounds.
   */
  def getForwardUpperBoundsIterable[E, D <: Domain[E], V](segment: Segment[E, D, V]): Iterable[Bound.Upper[E]] =
    segment.forwardIterable().map {
      case s: Segment.WithNext[E, D, V] => s.upperBound
      case _ => null
    }.filterNot(_ == null)
  
  /**
   * Returns upper bounds of subsequent segments from the given segment (inclusive).
   * @return list of upper bounds with its size.
   */
  def getForwardUpperBoundsList[E, D <: Domain[E], V](segment: Segment[E, D, V]): (List[Bound.Upper[E]], Int) = {
    var size = 0
    var list = List.empty[Bound.Upper[E]]
    @tailrec
    def loop(seg: Segment[E, D, V]): Unit = seg match {
      case seg: Segment.WithNext[E, D, V] =>
        size = size + 1
        list = list.appended(seg.upperBound)
        loop(seg.moveNext)
      case _ =>
    }
    loop(segment)
    (list, size)
  }

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
   *             bound          bound
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
  def getBoundSegments[E, D <: Domain[E], V](
    segment: Segment[E, D, ?], 
    seq: SegmentSeq[E, D, V]
  ): (Segment[E, D, V], Segment[E, D, V]) =
    segment match {
      case s: Segment.Inner[E, D, ?]    => (seq.getSegment(s.lowerBound), seq.getSegment(s.upperBound))
      case s: Segment.WithNext[E, D, ?] => (seq.firstSegment, seq.getSegment(s.upperBound))
      case s: Segment.WithPrev[E, D, ?] => (seq.getSegment(s.lowerBound), seq.lastSegment)
      case _                            => (seq.firstSegment, seq.lastSegment)
    }
}
