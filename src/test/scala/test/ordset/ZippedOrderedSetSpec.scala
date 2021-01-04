package test.ordset

import ordset.domain.Domain
import org.scalatest.funspec.AnyFunSpec

class ZippedOrderedSetSpec extends AnyFunSpec
  with SegmentSeqCases[Int, Domain[Int], Boolean]
  with SegmentSeqBehaviors[Int, Domain[Int], Boolean] {

  import ordset._
  import scala.collection.immutable.ArraySeq
  import instances.Int._
  import instances.Boolean._

  import scala.language.postfixOps
  import ordset.syntax.SetBuilderNotation._
  import ordset.syntax.BoundSyntax._
  import test.syntax.ArraySyntax._

  type Dom = Domain[Int]
  type SegmentSeq = SetSegmentSeq[Int, Dom]

  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]
  
  override val emptyCase: Option[TestCase] = TestCase.some(
    description =
      "empty set",
    sequence =
      // f union c:
      //                                              out
      // X--------------------------------------------------------------------------------------------------------X
      //
      // f = b intersection ~b:
      //                                              out
      // X--------------------------------------------------------------------------------------------------------X
      //
      // ~b:
      //     out             in                out                   in                  out                in
      // X--------0](0--------------10)[10--------------20)[20---------------30)[30--------------40)[40-----------X
      //
      // c = a intersection b:
      //                                              out
      // X--------------------------------------------------------------------------------------------------------X
      //
      // b:
      //     in             out                in                   out                  in                 out
      // X--------0](0--------------10)[10--------------20)[20---------------30)[30--------------40)[40-----------X
      //
      // a:
      //                                              out
      // X--------------------------------------------------------------------------------------------------------X
      ZippedOrderedSet.union(
        ZippedOrderedSet.intersection(
          // b
          new ArrayOrderedSet[Int, Dom](
            Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
            complement = false
          ),
          // a
          new ArrayOrderedSet[Int, Dom](
            ArraySeq.empty,
            complement = false
          )
        ),
        // f
        ZippedOrderedSet.intersection(
          // b
          new ArrayOrderedSet[Int, Dom](
            Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
            complement = false),
          // ~b
          new ArrayOrderedSet[Int, Dom](
            Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
            complement = true
          )
        )
      ),
    expected =
      (false forAll x) :: Nil
  )

  override val universalCase: Option[TestCase] = TestCase.some(
    description =
      "universal set",
    sequence =
      // f intersection c:
      //                                              in
      // X--------------------------------------------------------------------------------------------------------X
      //
      // f = b union ~b:
      //                                              in
      // X--------------------------------------------------------------------------------------------------------X
      //
      // ~b:
      //     out             in                out                   in                  out                in
      // X--------0](0--------------10)[10--------------20)[20---------------30)[30--------------40)[40-----------X
      //
      // c = a union b:
      //                                              in
      // X--------------------------------------------------------------------------------------------------------X
      //
      // b:
      //     in             out                in                   out                  in                 out
      // X--------0](0--------------10)[10--------------20)[20---------------30)[30--------------40)[40-----------X
      //
      // a:
      //                                              in
      // X--------------------------------------------------------------------------------------------------------X
      ZippedOrderedSet.intersection(
        // c
        ZippedOrderedSet.union(
          // b
          new ArrayOrderedSet[Int, Dom](
            Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
            complement = false),
          // a
          new ArrayOrderedSet[Int, Dom](
            ArraySeq.empty,
            complement = true
          )
        ),
        // f
        ZippedOrderedSet.union(
          // b
          new ArrayOrderedSet[Int, Dom](
            Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
            complement = false),
          // ~b
          new ArrayOrderedSet[Int, Dom](
            Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
            complement = true
          )
        )
      ),
    expected =
      (true forAll x) :: Nil
  )

  override val singleBoundedCase: Option[TestCase] = TestCase.some(
    description =
      "single bounded set",
    sequence =
      // b union c:
      //                          in                                                 out
      // X-----------------------------------------------0](0-----------------------------------------------------X
      //
      // c = a union b:
      //                          in                                                 out
      // X-----------------------------------------------0](0-----------------------------------------------------X
      //
      // b:
      //                          in                                                 out
      // X-----------------------------------------------0](0-----------------------------------------------------X
      //
      // a:
      //                                                 out
      // X--------------------------------------------------------------------------------------------------------X
      ZippedOrderedSet.union(
        // b
        new ArrayOrderedSet[Int, Dom](
          Array(0`](`).toImmutableArraySeq,
          complement = true),
        // c
        ZippedOrderedSet.union(
          // b
          new ArrayOrderedSet[Int, Dom](
            Array(0`](`).toImmutableArraySeq,
            complement = true),
          // a
          new ArrayOrderedSet[Int, Dom](
            ArraySeq.empty,
            complement = false)
        )
      ),
    expected =
      (true forAll x <=  0) ::
      (false forAll x > 0) ::
      Nil
  )

  override val multiBoundedCase: Option[TestCase] = TestCase.some(
    description =
      "multi bounded set",
    sequence =
      // c intersection d:
      //      in       out     in             out               in            out             in       out     in
      // X--------0](0-----5)[5--7)[7--------------------20](20----25)[25--------------35](35----40)[40--60)[60---X
      //
      // c:
      //              in                       out              in            out                    in
      // X-----------------------7)[7--------------------20](20----25)[25--------------35](35---------------------X
      //
      // d = a union b (merged):
      //     in        out                                        in                                  out      in
      // X--------0](0-----5)[5------------------------------------------------------------------40)[40--60)[60---X
      //
      // d = a union b:
      //     in        out      in       in         in               in         in        in          out      in
      // X--------0](0-----5)[5----10)[10--12](12-------20)[20---------------30)|(30-------------40)[40--60)[60---X
      //
      // b:
      //         out               in               out              in                    out                 in
      // X-----------------5)[5------------12](12-------20)[20----------------30](30---------------------60)[60---X
      //
      // a:
      //     in             out                in                   out                  in                 out
      // X--------0](0--------------10)[10--------------20)[20---------------30)[30--------------40)[40-----------X
      ZippedOrderedSet.intersection(
        // c
        new ArrayOrderedSet[Int, Dom](
          Array(7`)[`, 20`](`, 25`)[`, 35`](`).toImmutableArraySeq,
          complement = true
        ),
        // d
        ZippedOrderedSet.union(
          // a
          new ArrayOrderedSet[Int, Dom](
            Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
            complement = true
          ),
          // b
          new ArrayOrderedSet[Int, Dom](
            Array(5`)[`, 12`](`, 20`)[`, 30`](`, 60`)[`).toImmutableArraySeq,
            complement = false
          )
        )
      ),
    expected =
      (true  forAll x <= 0) ::
      (false forAll x >  0  & x <  5 ) ::
      (true  forAll x >= 5  & x <  7 ) ::
      (false forAll x >= 7  & x <= 20) ::
      (true  forAll x >  20 & x <  25) ::
      (false forAll x >= 25 & x <= 35) ::
      (true  forAll x >  35 & x <  40) ::
      (false forAll x >= 40 & x <  60) ::
      (true  forAll x >= 60) ::
      Nil
  )

  override val degenerateCase: Option[TestCase] = TestCase.some(
    description =
     "set with degenerate interval",
    sequence =
      // c intersection d:
      //     out   in        out                in         out         in        out               in
      // X--------0)|(0--------------10)[10--------------20)|(20---------------30)|(30----------------------------X
      //
      // d:
      //                                              in
      // X--------------------------------------------------------------------------------------------------------X
      //
      // c = a union b:
      //     out   in        out                in         out         in        out               in
      // X--------0)|(0--------------10)[10--------------20)|(20---------------30)|(30----------------------------X
      //
      // b:
      //     out   in                           out                                                in
      // X--------0)|(0---------------------------------------------------------30](------------------------------X
      //
      // a:
      //     out   in        out                in         out         in                          out
      // X--------0)|(0--------------10)[10--------------20)|(20---------------30)[30-----------------------------X
      ZippedOrderedSet.intersection(
        // d
        new ArrayOrderedSet[Int, Dom](
          ArraySeq.empty,
          complement = true),
        // c
        ZippedOrderedSet.union(
          // b
          new ArrayOrderedSet[Int, Dom](
            Array(0`)[`, 0`](`, 30`](`).toImmutableArraySeq,
            complement = false),
          // a
          new ArrayOrderedSet[Int, Dom](
            Array(0`)[`, 0`](`, 10`)[`, 20`)[`, 20`](`, 30`)[`).toImmutableArraySeq,
            complement = false)
        )
      ),
    expected =
      (false forAll x <  0) ::
      (true  forAll x >= 0  & x <= 0 ) ::
      (false forAll x >  0  & x <  10) ::
      (true  forAll x >= 10 & x <  20) ::
      (false forAll x >= 20 & x <= 20) ::
      (true  forAll x >  20 & x <  30) ::
      (false forAll x >= 30 & x <= 30) ::
      (true  forAll x >  30) ::
      Nil
  )

  describe("Zipped ordered set as a segment sequence") {

    it should behave like segmentsSupportMovePrevAndNext(
      emptyCase.get.description,
      emptyCase.get.sequence,
      emptyCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      universalCase.get.description,
      universalCase.get.sequence,
      universalCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      singleBoundedCase.get.description,
      singleBoundedCase.get.sequence,
      singleBoundedCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      multiBoundedCase.get.description,
      multiBoundedCase.get.sequence,
      multiBoundedCase.get.expected
    )

    it should behave like segmentsSupportMovePrevAndNext(
      degenerateCase.get.description,
      degenerateCase.get.sequence,
      degenerateCase.get.expected
    )

    it should behave like segmentsSupportMoveToBound(
      emptyCase.get.description,
      emptyCase.get.sequence,
      ( 10`)`, false forAll x) ::
      ( 15`[`, false forAll x) ::
      (-10`)`, false forAll x) ::
      (-15`[`, false forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      universalCase.get.description,
      universalCase.get.sequence,
      ( 10`)`, true forAll x) ::
      ( 15`[`, true forAll x) ::
      (-10`)`, true forAll x) ::
      (-15`[`, true forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      singleBoundedCase.get.description,
      singleBoundedCase.get.sequence,
      ( 10`)`, false forAll x >  0) ::
      ( 15`[`, false forAll x >  0) ::
      (-10`)`, true  forAll x <= 0) ::
      (-15`[`, true  forAll x <= 0) ::
      (  0`(`, false forAll x >  0) ::
      (  0`]`, true  forAll x <= 0) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      multiBoundedCase.get.description,
       multiBoundedCase.get.sequence,
       ( 7`[`, false forAll x >= 7  & x <= 20) ::
       (30`)`, false forAll x >= 25 & x <= 35) ::
       ( 0`)`, true  forAll x <= 0) ::
       (40`]`, false forAll x >= 40 & x <  60) ::
       (40`]`, false forAll x >= 40 & x <  60) ::
       (45`[`, false forAll x >= 40 & x <  60) ::
       ( 5`)`, false forAll x >  0  & x <  5 ) ::
       Nil
    )

    it should behave like segmentsSupportMoveToBound(
      degenerateCase.get.description,
      degenerateCase.get.sequence,
      (20`[`, false forAll x >= 20 & x <= 20) ::
      (20`]`, false forAll x >= 20 & x <= 20) ::
      (20`)`, true  forAll x >= 10 & x <  20) ::
      (30`(`, true  forAll x >  30) ::
      (40`)`, true  forAll x >  30) ::
      (-1`[`, false forAll x <  0 ) ::
      ( 0`]`, true  forAll x >= 0  & x <= 0 ) ::
      Nil
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      emptyCase.get.description,
      emptyCase.get.sequence,
      false forAll x,
      false forAll x
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      universalCase.get.description,
      universalCase.get.sequence,
      true forAll x,
      true forAll x
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      singleBoundedCase.get.description,
      singleBoundedCase.get.sequence,
      true forAll x <=  0,
      false forAll x > 0
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      multiBoundedCase.get.description,
      multiBoundedCase.get.sequence,
      true forAll x <=  0,
      true forAll x >= 60
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      degenerateCase.get.description,
      degenerateCase.get.sequence,
      false forAll x <  0,
      true forAll x > 30
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      emptyCase.get.description,
      emptyCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      universalCase.get.description,
      universalCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      singleBoundedCase.get.description,
      singleBoundedCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      multiBoundedCase.get.description,
      multiBoundedCase.get.sequence
    )

    it should behave like segmentsHaveNextAndPrevIndicators(
      degenerateCase.get.description,
      degenerateCase.get.sequence
    )
  }
}
