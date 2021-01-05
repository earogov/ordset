package test.ordset

import ordset.domain.Domain

trait SegmentSeqTestSuite[E, D <: Domain[E], W] {

  object Description {

    val emptySet: String = "empty set"
    val universalSet: String = "universal set"
    val singleBoundedSet: String = "single bounded set"
    val multiBoundedSet: String = "multi bounded set"
    val degenerateIntervalSet: String = "set with degenerate interval"
  }

  val emptyCase: Option[SegmentSeqTestCase[E, D, W]] = None
  val universalCase: Option[SegmentSeqTestCase[E, D, W]] = None
  val singleBoundedCase: Option[SegmentSeqTestCase[E, D, W]] = None
  val multiBoundedCase: Option[SegmentSeqTestCase[E, D, W]] = None
  val degenerateCase: Option[SegmentSeqTestCase[E, D, W]] = None

  def allCases: List[SegmentSeqTestCase[E, D, W]] =
    emptyCase.toList :::
    universalCase.toList :::
    singleBoundedCase.toList :::
    multiBoundedCase.toList :::
    degenerateCase.toList
}

