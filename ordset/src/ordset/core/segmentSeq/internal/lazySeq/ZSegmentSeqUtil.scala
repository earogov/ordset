package ordset.core.segmentSeq.internal.lazySeq

import ordset.core.domain.Domain

protected[ordset] object ZSegmentSeqUtil {
  
  /**
   * Returns `true`, if input sequence doesn't have lazy segments, i.e. is totally strict.
   */
  def isTotallyStrict[E, D[X] <: Domain[X], V](zippedSeq: ZSegmentSeq[E, D, V]): Boolean = {
    val firstSegment = zippedSeq.secondSeq.firstSegment
    firstSegment.value.isStrict && !firstSegment.hasNext
  }
}
