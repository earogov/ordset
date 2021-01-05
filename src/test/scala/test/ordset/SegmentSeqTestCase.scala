package test.ordset

import ordset.domain.Domain
import ordset.{IntervalRelation, SegmentSeq}

case class SegmentSeqTestCase[E, D <: Domain[E], W](
  description: String,
  sequence: SegmentSeq[E, D, W],
  expected: Seq[IntervalRelation[E, D, W]]
)

object SegmentSeqTestCase {

  def some[E, D <: Domain[E], W](
    description: String,
    sequence: SegmentSeq[E, D, W],
    expected: Seq[IntervalRelation[E, D, W]]
  ): Option[SegmentSeqTestCase[E, D, W]] =
    Some(SegmentSeqTestCase(description, sequence, expected))
}
