package test.ordset

import org.scalatest.funspec.AnyFunSpec

class ArrayOrderedSetSpec extends AnyFunSpec
  with SegmentSeqCases[Int, Boolean]
  with SegmentSeqBehaviors[Int, Boolean] {

  import ordset._
  import scala.collection.immutable.ArraySeq
  import cats.kernel.instances.int._

  import scala.language.postfixOps
  import ordset.syntax.SetBuilderNotation._
  import ordset.syntax.BoundSyntax._
  import test.syntax.ArraySyntax._

  type SegmentSeq = SetSegmentSeq[Int]

  override val emptyCase: Option[SegmentSeq] = Some(
    new ArrayOrderedSet[Int](ArraySeq.empty, complement = false)
  )

  override val universalCase: Option[SegmentSeq] = Some(
    new ArrayOrderedSet[Int](ArraySeq.empty, complement = true)
  )

  override val singleBoundedCase: Option[SegmentSeq] = Some(
    new ArrayOrderedSet[Int](Array(0`](`).toImmutableArraySeq, complement = true)
  )

  override val multiBoundedCase: Option[SegmentSeq] = Some(
    new ArrayOrderedSet[Int](Array(0`)[`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq, complement = false)
  )

  override val degenerateCase: Option[SegmentSeq] = Some(
    new ArrayOrderedSet[Int](Array(0`)[`, 0`](`, 10`)[`, 20`)[`, 20`](`, 30`)[`).toImmutableArraySeq, complement = false)
  )

  describe("Array based ordered set as a segment sequence") {

    it should behave like supportMovePrevAndNextForSegments("empty set",
      emptyCase.get.firstSegment,
      (false forAll x) :: Nil
    )

    it should behave like supportMovePrevAndNextForSegments("universal set",
      universalCase.get.firstSegment,
      (true forAll x) :: Nil
    )

    it should behave like supportMovePrevAndNextForSegments("single bounded set",
      singleBoundedCase.get.firstSegment,
      (true forAll x <=  0) ::
      (false forAll x > 0) ::
      Nil
    )

    it should behave like supportMovePrevAndNextForSegments("multi bounded set",
      multiBoundedCase.get.firstSegment,
      (false forAll x <  0) ::
      (true  forAll x >= 0  & x < 10) ::
      (false forAll x >= 10 & x < 20) ::
      (true  forAll x >= 20 & x < 30) ::
      (false forAll x >= 30 & x < 40) ::
      (true  forAll x >= 40) ::
      Nil
    )

    it should behave like supportMovePrevAndNextForSegments("set with degenerate interval",
      degenerateCase.get.firstSegment,
      (false forAll x <  0) ::
      (true  forAll x >= 0  & x <= 0) ::
      (false forAll x >  0  & x <  10) ::
      (true  forAll x >= 10 & x <  20) ::
      (false forAll x >= 20 & x <= 20) ::
      (true  forAll x >  20 & x <  30) ::
      (false forAll x >= 30) ::
      Nil
    )

    it should behave like supportMoveToForSegments("empty set",
      emptyCase.get.firstSegment,
      ( 10`)`, false forAll x) ::
      ( 15`[`, false forAll x) ::
      (-10`)`, false forAll x) ::
      (-15`[`, false forAll x) ::
      Nil
    )

    it should behave like supportMoveToForSegments("universal set",
      universalCase.get.firstSegment,
      ( 10`)`, true forAll x) ::
      ( 15`[`, true forAll x) ::
      (-10`)`, true forAll x) ::
      (-15`[`, true forAll x) ::
      Nil
    )

    it should behave like supportMoveToForSegments("single bounded set",
      singleBoundedCase.get.firstSegment,
      ( 10`)`, false forAll x >  0) ::
      ( 15`[`, false forAll x >  0) ::
      (-10`)`, true  forAll x <= 0) ::
      (-15`[`, true  forAll x <= 0) ::
      (  0`(`, false forAll x >  0) ::
      (  0`]`, true  forAll x <= 0) ::
      Nil
    )

    it should behave like supportMoveToForSegments("multi bounded set",
      multiBoundedCase.get.firstSegment,
      (10`)`, true  forAll x >= 0  & x < 10) ::
      (10`)`, true  forAll x >= 0  & x < 10) ::
      (30`[`, false forAll x >= 30 & x < 40) ::
      (40`)`, false forAll x >= 30 & x < 40) ::
      (40`[`, true  forAll x >= 40) ::
      (45`[`, true  forAll x >= 40) ::
      (25`[`, true  forAll x >= 20 & x < 30) ::
      (-5`[`, false forAll x <  0) ::
      Nil
    )

    it should behave like supportMoveToForSegments("set with degenerate interval",
      degenerateCase.get.firstSegment,
      ( 0`]`, true  forAll x >= 0  & x <= 0) ::
      (20`(`, true  forAll x >  20 & x <  30) ::
      (-5`]`, false forAll x <  0) ::
      (45`[`, false forAll x >= 30) ::
      Nil
    )
  }
}