package test.ordset

import ordset.ContinuousDomain
import org.scalatest.funspec.AnyFunSpec

class ZippedOrderedSetSpec extends AnyFunSpec
with SegmentSeqCases[Int, ContinuousDomain[Int], Boolean]
with SegmentSeqBehaviors[Int, ContinuousDomain[Int], Boolean] {

  import ordset._
  import scala.collection.immutable.ArraySeq
  import OrderWithDir._

  import scala.language.postfixOps
  import ordset.syntax.SetBuilderNotation._
  import ordset.syntax.BoundSyntax._
  import test.syntax.ArraySyntax._

  type Domain = ContinuousDomain[Int]
  type SegmentSeq = SetSegmentSeq[Int, Domain]

  implicit val domain: Domain = ContinuousDomain()

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
        new ArrayOrderedSet[Int, Domain](
          Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
          complement = false
        ),
        // a
        new ArrayOrderedSet[Int, Domain](
          ArraySeq.empty,
          complement = false
        )
      ),
      // f
      ZippedOrderedSet.intersection(
        // b
        new ArrayOrderedSet[Int, Domain](
          Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
          complement = false),
        // ~b
        new ArrayOrderedSet[Int, Domain](
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
        new ArrayOrderedSet[Int, Domain](
          Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
          complement = false),
        // a
        new ArrayOrderedSet[Int, Domain](
          ArraySeq.empty,
          complement = true
        )
      ),
      // f
      ZippedOrderedSet.union(
        // b
        new ArrayOrderedSet[Int, Domain](
          Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
          complement = false),
        // ~b
        new ArrayOrderedSet[Int, Domain](
          Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
          complement = true
        )
      )
    )
  )

  override val singleBoundedCase: Option[SegmentSeq] = None

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
      new ArrayOrderedSet[Int, Domain](
        Array(7`)[`, 20`](`, 25`)[`, 35`](`).toImmutableArraySeq,
        complement = true
      ),
      // d
      ZippedOrderedSet.union(
        // a
        new ArrayOrderedSet[Int, Domain](
          Array(0`](`, 10`)[`, 20`)[`, 30`)[`, 40`)[`).toImmutableArraySeq,
          complement = true
        ),
        // b
        new ArrayOrderedSet[Int, Domain](
          Array(5`)[`, 12`](`, 20`)[`, 30`](`, 60`)[`).toImmutableArraySeq,
          complement = false
        )
      )
    )
  )

  override val degenerateCase: Option[SegmentSeq] = None

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

    it should behave like segmentsSupportMoveToBound(
      "empty set",
      emptyCase.get.firstSegment,
      ( 10`)`, false forAll x) ::
      ( 15`[`, false forAll x) ::
      (-10`)`, false forAll x) ::
      (-15`[`, false forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "universal set",
      universalCase.get.firstSegment,
      ( 10`)`, true forAll x) ::
      ( 15`[`, true forAll x) ::
      (-10`)`, true forAll x) ::
      (-15`[`, true forAll x) ::
      Nil
    )

    it should behave like segmentsSupportMoveToBound(
      "multi bounded set",
       multiBoundedCase.get.firstSegment,
       ( 7`[`, false forAll x >= 7  & x <= 20) ::
       (30`)`, false forAll x >= 25 & x <= 35) ::
       ( 0`)`, true  forAll x <= 0) ::
       (40`]`, false forAll x >= 40 & x <  60) ::
       (40`]`, false forAll x >= 40 & x <  60) ::
       (45`[`, false forAll x >= 40 & x <  60) ::
       ( 5`)`, false forAll x >  0  & x <  5 ) ::
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
      "multi bounded set",
      multiBoundedCase.get,
      true forAll x <= 0,
      true forAll x >= 60
    )

    it should behave like segmentsHaveNextAndPrevIndicators("empty set", emptyCase.get)

    it should behave like segmentsHaveNextAndPrevIndicators("universal set", universalCase.get)

    it should behave like segmentsHaveNextAndPrevIndicators("multi bounded set", multiBoundedCase.get)
  }
}
