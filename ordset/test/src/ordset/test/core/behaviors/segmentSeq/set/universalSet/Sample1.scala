package ordset.test.core.behaviors.segmentSeq.set.universalSet

import ordset.core.domain.Domain
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.core.{Bound, ExtendedBound}
import ordset.core.segmentSeq.*
import ordset.test.Label.*
import ordset.test.core.behaviors.TestTuple
import ordset.test.core.behaviors.segmentSeq._
import ordset.test.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample1[D[X] <: Domain[X]]
  extends SegmentMoveToBoundTest[Int, D, Boolean]
    with SegmentContainsTest[Int, D, Boolean]
    with SegmentRestrictBoundTest[Int, D, Boolean]
    with SegmentSeqPrependTest[Int, D, Boolean]
    with SegmentSeqAppendTest[Int, D, Boolean]
    with SegmentSeqSliceTest[Int, D, Boolean]
    with SegmentPatchTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean, SegmentSeq[Int, D, Boolean]] =>

  override val sample: String = "1"

  override val complementary: Boolean = true

  override val reference: Seq[GenIntervalRelation] =
    (true forAll x) ::
    Nil

  override lazy val moveToBoundCases: Seq[(ExtendedBound[Int], GenIntervalRelation)] =
    (ExtendedBound.BelowAll, true forAll x) ::
    (ExtendedBound.AboveAll, true forAll x) ::
    ( 10`)`, true forAll x) ::
    ( 15`[`, true forAll x) ::
    (-10`)`, true forAll x) ::
    (-15`[`, true forAll x) ::
    Nil

  override lazy val containsCases: Seq[SegmentContainsTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentContainsTest.TestCase(
        bound = 0`[`,
        includedBounds = List(ExtendedBound.BelowAll, ExtendedBound.AboveAll, -10`[`, 10`)`),
        excludedBounds = List()
      )
    )

  override lazy val restrictCases: Seq[SegmentRestrictBoundTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentRestrictBoundTest.TestCase(
        bound = 0`]`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, ExtendedBound.BelowAll),
          TestTuple(ExtendedBound.AboveAll, ExtendedBound.AboveAll),
          TestTuple(0`)`, 0`)`),
          TestTuple(0`[`, 0`[`),
          TestTuple(-10`(`, -10`(`),
          TestTuple(-10`]`, -10`]`)
        )
      )
    )
  
  override lazy val prependCases: Seq[SegmentSeqPrependTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X------------------true------------------------X
        // other:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + label("A"),
          factoryTuple._2.buildUniform(false),
          (false forAll x) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X------------------true------------------------X
        // result:
        // X------------------true------------------------X
        //
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + label("B"),
          factoryTuple._2.buildUniform(true),
          reference
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X--------true---------](--------false----------X
        //                       0
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + label("C"),
          factoryTuple._2.unsafeBuild(ArraySeq(0`](`), complementary = true),
          (true  forAll x <= 0) ::
          (false forAll x >  0) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X--------false--------](---------true----------X
        //                       0
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + label("D"),
          factoryTuple._2.unsafeBuild(ArraySeq(0`](`), complementary = false),
          (false forAll x <= 0) ::
          (true  forAll x >  0) ::
          Nil
        )
      )
    }
  }

  override lazy val prependBelowBoundCases: Seq[SegmentSeqPrependTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // other:
        // X-------------------true-----------------------X
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("A1"),
          0 `]`,
          factoryTuple._2.buildUniform(true),
          reference
        ),
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // other:
        // X-------------------false----------------------X
        // result:
        // X----------false---------](---------true-------X
        //                           0
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("B1"),
          0`]`,
          factoryTuple._2.buildUniform(false),
          (false forAll x <= 0) ::
          (true  forAll x >  0) ::
          Nil
        ),
        // current:
        //
        // bound
        // X
        // X------------------true------------------------X
        //
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X------------------true------------------------X
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("C1"),
          ExtendedBound.BelowAll,
          factoryTuple._2.unsafeBuild(ArraySeq(-5`](`), complementary = false),
          reference
        ),
        // current:
        //                                            bound
        //                                                X
        // X------------------true------------------------X
        //
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X-f-](----------------true---------------------X
        //     -5
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + label("C2"),
          ExtendedBound.AboveAll,
          factoryTuple._2.unsafeBuild(ArraySeq(-5`](`), complementary = false),
          (false forAll x <= -5) ::
          (true  forAll x >  -5) ::
          Nil
        )
      )
    }
  }

  override lazy val appendCases: Seq[SegmentSeqAppendTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X------------------true------------------------X
        // other:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + label("A"),
          factoryTuple._2.buildUniform(false),
          (false forAll x) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X------------------true------------------------X
        // result:
        // X------------------true------------------------X
        //
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + label("B"),
          factoryTuple._2.buildUniform(true),
          reference
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X--------true---------](--------false----------X
        //                       0
        // result:
        // X--------true---------](--------false----------X
        //                       0
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + label("C"),
          factoryTuple._2.unsafeBuild(ArraySeq(0`](`), complementary = true),
          (true  forAll x <= 0) ::
          (false forAll x >  0) ::
          Nil
        ),
        // current:
        // X------------------true------------------------X
        // other:
        // X--------false--------](---------true----------X
        //                       0
        // result:
        // X--------false--------](---------true----------X
        //                       0
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + label("D"),
          factoryTuple._2.unsafeBuild(ArraySeq(0`](`), complementary = false),
          (false forAll x <= 0) ::
          (true  forAll x >  0) ::
          Nil
        )
      )
    }
  }

  override lazy val appendAboveBoundCases: Seq[SegmentSeqAppendTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // other:
        // X-------------------true-----------------------X
        // result:
        // X-------------------true-----------------------X
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("A1"),
          0`]`,
          factoryTuple._2.buildUniform(true),
          reference
        ),
        // current:
        //                         bound
        //                           ]
        // X-------------------true-----------------------X
        // other:
        // X-------------------false----------------------X
        // result:
        // X-----------true----------](--------false-------X
        //                           0
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("B1"),
          0`]`,
          factoryTuple._2.buildUniform(false),
          (true  forAll x <= 0) ::
          (false forAll x >  0) ::
          Nil
        ),
        // current:
        //
        // bound
        // X
        // X------------------true------------------------X
        //
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X-f-](----------------true---------------------X
        //     -5
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("C1"),
          ExtendedBound.BelowAll,
          factoryTuple._2.unsafeBuild(ArraySeq(-5`](`), complementary = false),
          (false forAll x <= -5) ::
          (true  forAll x >  -5) ::
          Nil
        ),
        // current:
        //
        //                                            bound
        //                                                X
        // X------------------true------------------------X
        //
        // other:
        // X-f-](----------------true---------------------X
        //     -5
        //
        // result:
        // X------------------false-----------------------X
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + label("C2"),
          ExtendedBound.AboveAll,
          factoryTuple._2.unsafeBuild(ArraySeq(-5`](`), complementary = false),
          reference
        )
      )
    }
  }
  
  override lazy val sliceCases: Seq[SegmentSeqSliceTest.TestCase[Int, D, Boolean]] =
    List(
      // current: 
      //                     bound
      //                       )
      // X------------------true------------------------X
      //
      // takeBelow:
      // X------------------true------------------------X
      // 
      // takeAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        0`)`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       ]
      // X------------------true------------------------X
      //
      // takeBelow:
      // X------------------true------------------------X
      // 
      // takeAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        0`]`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       [
      // X------------------true------------------------X
      //
      // takeBelow:
      // X------------------true------------------------X
      // 
      // takeAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        0`[`,
        reference,
        reference
      ),
      // current: 
      //                     bound
      //                       (
      // X------------------true------------------------X
      //
      // takeBelow:
      // X------------------true------------------------X
      // 
      // takeAbove:
      // X------------------true------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        0`(`,
        reference,
        reference
      ),
      // current:
      //
      // bound
      // X
      // X-----------------true-------------------------X
      //
      // takeBelow:
      // X-----------------true-------------------------X
      //
      // takeAbove:
      // X-----------------true-------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        ExtendedBound.BelowAll,
        reference,
        reference
      ),
      // current:
      //                                            bound
      //                                                X
      // X-----------------true-------------------------X
      //
      // takeBelow:
      // X-----------------true-------------------------X
      //
      // takeAbove:
      // X-----------------true-------------------------X
      //
      SegmentSeqSliceTest.TestCase(
        ExtendedBound.AboveAll,
        reference,
        reference
      )
    )

  override lazy val patchCases: Seq[SegmentPatchTest.TestCase[Int, D, Boolean]] =
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //                patched segment
        // X------------------true------------------------X
        // patch:
        // X------------------false-----------------------X
        // result:
        // X------------------false-----------------------X
        //
        SegmentPatchTest.TestCase(
          factoryTuple._1 + label("A1"),
          0 `[`,
          factoryTuple._2.buildUniform(false),
          (false forAll x) ::
          Nil
        ),
        // current:
        //                patched segment
        // X------------------true------------------------X
        // patch:
        // X------------------true------------------------X
        // result:
        // X------------------true------------------------X
        //
        SegmentPatchTest.TestCase(
          factoryTuple._1 + label("A2"),
          0 `[`,
          factoryTuple._2.buildUniform(true),
          reference
        ),
        // current:
        //                patched segment
        // X-------------------true-----------------------X
        // patch:
        // X---------false------)[----------true----------X
        //                      0
        // result:
        // X---------false------)[----------true----------X
        //                      0
        SegmentPatchTest.TestCase(
          factoryTuple._1 + label("B1"),
          0 `[`,
          factoryTuple._2.unsafeBuild(ArraySeq(0 `)[`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0) ::
          Nil
        )
      )
    }
}
