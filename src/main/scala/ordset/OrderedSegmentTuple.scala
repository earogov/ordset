package ordset

trait OrderedSegmentTuple[E, D <: Domain[E], +V] {

  def left: Segment[E, D, V]

  def right: Segment[E, D, V]

  def forward: Segment[E, D, V]

  def backward: Segment[E, D, V]
}

object OrderedSegmentTuple {

  def fromUnordered[E, D <: Domain[E], V](left: Segment[E, D, V], right: Segment[E, D, V]
  ): OrderedSegmentTuple[E, D, V] = new LazyTuple(left, right)

  def fromOrdered[E, D <: Domain[E], V](forward: Segment.Last[E, D, V], backward: Segment[E, D, V]
  ): OrderedSegmentTuple[E, D, V] = new EagerTuple(forward, backward)

  private class LazyTuple[E, D <: Domain[E], V](
    override val left: Segment[E, D, V],
    override val right: Segment[E, D, V],
  ) extends OrderedSegmentTuple[E, D, V] {

    private lazy val choice: BinaryChoice = SegmentBinaryChoice.forwardChoice(left, right)

    override def forward: Segment[E, D, V] = SegmentBinaryChoice.get(left, right, choice)

    override def backward: Segment[E, D, V] = SegmentBinaryChoice.get(right, left, choice)
  }

  private class EagerTuple[E, D <: Domain[E], V](
    override val forward: Segment[E, D, V],
    override val backward: Segment[E, D, V],
  ) extends OrderedSegmentTuple[E, D, V] {

    override def left: Segment[E, D, V] = forward

    override def right: Segment[E, D, V] = backward
  }
}
