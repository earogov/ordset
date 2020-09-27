package test.ordset

import ordset.domain.Domain

trait SegmentSeqCases[E, D <: Domain[E], W] {
  import ordset._

  val emptyCase: Option[SegmentSeq[E, D, W]]
  val universalCase: Option[SegmentSeq[E, D, W]]
  val singleBoundedCase: Option[SegmentSeq[E, D, W]]
  val multiBoundedCase: Option[SegmentSeq[E, D, W]]
  val degenerateCase: Option[SegmentSeq[E, D, W]]
}

