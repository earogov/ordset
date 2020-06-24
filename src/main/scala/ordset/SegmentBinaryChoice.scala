package ordset

object SegmentBinaryChoice {

  def get[E, D <: Domain[E], V](left: Segment[E, D, V], right: Segment[E, D, V], choice: BinaryChoice
  ): Segment[E, D, V] = if (BinaryChoice.isFirst(choice)) left else right

  def forwardChoice[E, D <: Domain[E]](left: Segment[E, D, Any], right: Segment[E, D, Any]): BinaryChoice.Type =
    if (left.domain.segmentOrd.compare(left, right) >= 0) FistOfTwo else SecondOfTwo

  def backwardChoice[E, D <: Domain[E]](left: Segment[E, D, Any], right: Segment[E, D, Any]): BinaryChoice.Type =
    if (left.domain.segmentOrd.compare(left, right) >= 0) SecondOfTwo else FistOfTwo

  def getForward[E, D <: Domain[E], V](left: Segment[E, D, V], right: Segment[E, D, V]): Segment[E, D, V] =
    if (left.domain.segmentOrd.compare(left, right) >= 0) left else right

  def getBackward[E, D <: Domain[E], V](left: Segment[E, D, V], right: Segment[E, D, V]): Segment[E, D, V] =
    if (left.domain.segmentOrd.compare(left, right) >= 0) right else left
}
