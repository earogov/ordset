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
  
  override val emptyCase: Option[SegmentSeq] = Some(
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
    )
  )

  override val universalCase: Option[SegmentSeq] = Some(
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
    )
  )

  override val singleBoundedCase: Option[SegmentSeq] = Some(
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
    )
  )

  override val multiBoundedCase: Option[SegmentSeq] = Some(
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
    )
  )

  override val degenerateCase: Option[SegmentSeq] = Some(
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
    )
  )

  describe("Zipped ordered set as a segment sequence") {

    it should behave like segmentsSupportMovePrevAndNext(
      "empty set",
      emptyCase.get,
      (false forAll x) :: Nil
    )

    it should behave like segmentsSupportMovePrevAndNext(
      "universal set",
      universalCase.get,
      (true forAll x) :: Nil
    )

    it should behave like segmentsSupportMovePrevAndNext(
      "single bounded set",
      singleBoundedCase.get,
      (true forAll x <=  0) ::
      (false forAll x > 0) ::
      Nil
    )

    it should behave like segmentsSupportMovePrevAndNext(
      "multi bounded set",
      multiBoundedCase.get,
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

    it should behave like segmentsSupportMovePrevAndNext(
      "set with degenerate interval",
      degenerateCase.get,
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

    it should behave like segmentsSupportMoveToBound(
      "empty set",
      emptyCase.get,
      ( 10`)`, false forAll x) ::
      ( 15`[`, false forAll x) ::
      (-10`)`, false forAll x) ::
      (-15`[`, false forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "universal set",
      universalCase.get,
      ( 10`)`, true forAll x) ::
      ( 15`[`, true forAll x) ::
      (-10`)`, true forAll x) ::
      (-15`[`, true forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "single bounded set",
      singleBoundedCase.get,
      ( 10`)`, false forAll x >  0) ::
      ( 15`[`, false forAll x >  0) ::
      (-10`)`, true  forAll x <= 0) ::
      (-15`[`, true  forAll x <= 0) ::
      (  0`(`, false forAll x >  0) ::
      (  0`]`, true  forAll x <= 0) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "multi bounded set",
       multiBoundedCase.get,
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
      "set with degenerate interval",
      degenerateCase.get,
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
      "empty set",
      emptyCase.get,
      false forAll x,
      false forAll x
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "universal set",
      universalCase.get,
      true forAll x,
      true forAll x
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "single bounded set",
      singleBoundedCase.get,
      true forAll x <=  0,
      false forAll x > 0
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "multi bounded set",
      multiBoundedCase.get,
      true forAll x <=  0,
      true forAll x >= 60
    )

    it should behave like segmentsSupportMoveToFirstAndLast(
      "set with degenerate interval",
      degenerateCase.get,
      false forAll x <  0,
      true forAll x > 30
    )

    it should behave like segmentsHaveNextAndPrevIndicators("empty set", emptyCase.get)

    it should behave like segmentsHaveNextAndPrevIndicators("universal set", universalCase.get)

    it should behave like segmentsHaveNextAndPrevIndicators("single bounded set", singleBoundedCase.get)

    it should behave like segmentsHaveNextAndPrevIndicators("multi bounded set", multiBoundedCase.get)

    it should behave like segmentsHaveNextAndPrevIndicators("set with degenerate interval", degenerateCase.get)
  }
}
