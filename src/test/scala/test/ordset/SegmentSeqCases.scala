package test.ordset

import ordset.domain.Domain

trait SegmentSeqCases[E, D <: Domain[E], W] {
  import ordset._

  case class TestCase(
    sequence: SegmentSeq[E, D, W],
    expected: Seq[IntervalMapping[E, D, W]]
  )

  object TestCase {

    def some(sequence: SegmentSeq[E, D, W], expected: Seq[IntervalMapping[E, D, W]]): Option[TestCase] =
      Some(TestCase(sequence, expected))
  }

  val emptyCase: Option[TestCase]
  val universalCase: Option[TestCase]
  val singleBoundedCase: Option[TestCase]
  val multiBoundedCase: Option[TestCase]
  val degenerateCase: Option[TestCase]
}

