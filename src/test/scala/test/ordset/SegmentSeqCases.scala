package test.ordset

trait SegmentSeqCases[E, W] {
  import ordset._

  val emptyCase: Option[SegmentSeq[E, W]]
  val universalCase: Option[SegmentSeq[E, W]]
  val singleBoundedCase: Option[SegmentSeq[E, W]]
  val multiBoundedCase: Option[SegmentSeq[E, W]]
  val degenerateCase: Option[SegmentSeq[E, W]]
}

