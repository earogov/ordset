package ordset.core.segmentSeq

/**
 * Exception during creating or using [[SegmentSeq]].
 */
class SegmentSeqException(message: String, cause: Throwable | Null) extends RuntimeException(message, cause)

object SegmentSeqException {

  def apply(message: String, cause: Throwable): SegmentSeqException = new SegmentSeqException(message, cause)

  def apply(message: String): SegmentSeqException = new SegmentSeqException(message, null)

  def apply(cause: Throwable): SegmentSeqException = new SegmentSeqException("", cause)
  
  def seqBuildFailed(cause: Throwable): SegmentSeqException = 
    new SegmentSeqException("Unable to build segment sequence", cause)
}
