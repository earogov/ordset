package ordset.core.internal

import ordset.core.SegmentLike
import ordset.core.domain.Domain

protected[ordset] object SegmentSeqExceptionUtil {

  @throws[NoSuchElementException]
  def throwNoNextSegment[E, D <: Domain[E], V](segment: SegmentLike[E, D, V]): Nothing = {
    throw new NoSuchElementException(s"Segment $segment doesn't have next segment.")
  }

  @throws[NoSuchElementException]
  def throwNoPrevSegment[E, D <: Domain[E], V](segment: SegmentLike[E, D, V]): Nothing = {
    throw new NoSuchElementException(s"Segment $segment doesn't have previous segment.")
  }
}
