package ordset.core

import ordset.core.domain.Domain

import scala.annotation.tailrec

object SegmentSeqOps {

  /**
   * Returns upper bounds of subsequent segments from the given segment (inclusive).
   * @return list of upper bounds with its size
   */
  def getForwardBoundsList[E, D <: Domain[E], W](segment: Segment[E, D, W]): (List[Bound.Upper[E]], Int) = {
    var size = 0
    var list = List.empty[Bound.Upper[E]]
    @tailrec
    def loop(seg: Segment[E, D, W]): Unit = seg match {
      case seg: Segment.WithNext[E, D, W] =>
        size = size + 1
        list = list.appended(seg.upperBound)
        loop(seg.moveNext)
      case _ =>
    }
    loop(segment)
    (list, size)
  }
}
