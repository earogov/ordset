package test.ordset

import ordset.domain.Domain

trait SegmentSeqCases[E, D <: Domain[E], W] {
  import ordset._

  case class TestCase(
    description: String,
    sequence: SegmentSeq[E, D, W],
    expected: Seq[IntervalRelation[E, D, W]]
  )

  object TestCase {

    def some(
      description: String,
      sequence: SegmentSeq[E, D, W],
      expected: Seq[IntervalRelation[E, D, W]]
    ): Option[TestCase] =
      Some(TestCase(description, sequence, expected))
  }

  val emptyCase: Option[TestCase] = None
  val universalCase: Option[TestCase] = None
  val singleBoundedCase: Option[TestCase] = None
  val multiBoundedCase: Option[TestCase] = None
  val degenerateCase: Option[TestCase] = None
}

