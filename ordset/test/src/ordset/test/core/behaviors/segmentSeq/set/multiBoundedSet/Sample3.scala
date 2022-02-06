package ordset.test.core.behaviors.segmentSeq.set.multiBoundedSet

import ordset.core.domain.Domain
import ordset.core.set.NonuniformTreapOrderedSet
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.core.{Bound, ExtendedBound, SegmentSeq}
import ordset.util.label.Label
import ordset.test.core.Labels
import ordset.test.core.behaviors.TestTuple
import ordset.test.core.behaviors.segmentSeq._
import ordset.test.core.samples.segmentSeq.SegmentSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait Sample3[D[X] <: Domain[X]]
  extends SegmentMoveToBoundTest[Int, D, Boolean]
    with SegmentContainsTest[Int, D, Boolean]
    with SegmentRestrictBoundTest[Int, D, Boolean]
    with SegmentSeqPrependTest[Int, D, Boolean]
    with SegmentSeqAppendTest[Int, D, Boolean]
    with SegmentSeqSliceTest[Int, D, Boolean]
    with SegmentPatchTest[Int, D, Boolean] {
  self: SegmentSeqSample[Int, D, Boolean, SegmentSeq[Int, D, Boolean]] =>

  override val sample: String = "3"
  
  override val complementary: Boolean = false

  override val reference: Seq[GenIntervalRelation] =
    (false forAll x <  0) ::
    (true  forAll x >= 0  & x <  10) ::
    (false forAll x >= 10 & x <  20) ::
    (true  forAll x >= 20 & x <  30) ::
    (false forAll x >= 30 & x <  40) ::
    (true  forAll x >= 40 & x <= 50) ::
    (false forAll x >  50 & x <= 60) ::
    (true  forAll x >  60 & x <  70) ::
    (false forAll x >= 70 & x <  80) ::
    (true  forAll x >= 80) ::
    Nil

  override lazy val moveToBoundCases: Seq[(ExtendedBound[Int], GenIntervalRelation)] =
    (10`)`, true  forAll x >= 0  & x <  10) ::
    (10`)`, true  forAll x >= 0  & x <  10) ::
    (30`[`, false forAll x >= 30 & x <  40) ::
    (40`)`, false forAll x >= 30 & x <  40) ::
    (40`[`, true  forAll x >= 40 & x <= 50) ::
    (45`[`, true  forAll x >= 40 & x <= 50) ::
    (25`[`, true  forAll x >= 20 & x <  30) ::
    (ExtendedBound.AboveAll, true  forAll x >= 80) ::
    (-5`[`, false forAll x <  0) ::
    (80`[`, true  forAll x >= 80) ::
    (70`)`, true  forAll x >  60 & x <  70) ::
    (75`[`, false forAll x >= 70 & x <  80) ::
    (70`]`, false forAll x >= 70 & x <  80) ::
    (70`[`, false forAll x >= 70 & x <  80) ::
    (ExtendedBound.BelowAll, false forAll x <  0) ::
    Nil

  override lazy val containsCases: Seq[SegmentContainsTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentContainsTest.TestCase(
        bound = 0`)`,
        includedBounds = List(ExtendedBound.BelowAll, -10`[`, 0`)`),
        excludedBounds = List(ExtendedBound.AboveAll, 0`[`, 0`]`, 0`(`, 10`[`)
      ),
      SegmentContainsTest.TestCase(
        bound = 55`]`,
        includedBounds = List(50`(`, 55`(`, 60`[`, 60`]`, 60`)`),
        excludedBounds = List(ExtendedBound.BelowAll, ExtendedBound.AboveAll, 50`]`, 50`[`, 50`)`, 60`(`, 70`]`)
      ),
      SegmentContainsTest.TestCase(
        bound = 90`]`,
        includedBounds = List(ExtendedBound.AboveAll, 80`]`, 80`[`, 80`(`, 100`]`),
        excludedBounds = List(ExtendedBound.BelowAll, 80`)`, 79`]`)
      )
    )

  override lazy val restrictCases: Seq[SegmentRestrictBoundTest.TestCase[Int, D, Boolean]] =
    List(
      SegmentRestrictBoundTest.TestCase(
        bound = -10`[`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, ExtendedBound.BelowAll),
          TestTuple(ExtendedBound.AboveAll, 0`)`),
          TestTuple(-10`]`, -10`]`),
          TestTuple(0`]`, 0`)`),
          TestTuple(10`(`, 0`)`)
        )
      ),
      SegmentRestrictBoundTest.TestCase(
        bound = 65`[`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, 60`(`),
          TestTuple(ExtendedBound.AboveAll, 70`)`),
          TestTuple(-10`]`, 60`(`),
          TestTuple(100`]`, 70`)`),
          TestTuple(60`(`, 60`(`),
          TestTuple(60`)`, 60`(`),
          TestTuple(60`]`, 60`(`),
          TestTuple(60`[`, 60`(`),
          TestTuple(70`)`, 70`)`),
          TestTuple(70`(`, 70`)`),
          TestTuple(70`[`, 70`)`),
          TestTuple(70`]`, 70`)`)
        )
      ),
      SegmentRestrictBoundTest.TestCase(
        bound = 100`[`,
        restrictedBounds = List(
          TestTuple(ExtendedBound.BelowAll, 80`[`),
          TestTuple(ExtendedBound.AboveAll, ExtendedBound.AboveAll),
          TestTuple(-10`]`, 80`[`),
          TestTuple(100`]`, 100`]`),
          TestTuple(80`(`, 80`(`),
          TestTuple(80`)`, 80`[`),
          TestTuple(80`]`, 80`]`)
        )
      )
    )

  override lazy val prependCases: Seq[SegmentSeqPrependTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // X------f------)[--t--)[ .............Seq1............. X
        //               0      10
        // other:
        // X------------------------false-------------------------X
        //
        // result:
        // X------f------)[--t--)[ .............Seq1............. X
        //               0      10
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.buildUniform(false),
          reference
        ),
        // current:
        // X------f------)[--t--)[ .............Seq1............. X
        //               0      10
        // other:
        // X------------------------true--------------------------X
        //
        // result:
        // X----------t---------)[ .............Seq1............. X
        //                      10
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.buildUniform(true),
          (true  forAll x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80) ::
          Nil
        ),
        // current:
        // X------f------)[--t--)[ .............Seq1............. X
        //               0      10
        // other:
        // X---------f-------)[-------------true------------------X
        //                   5
        // result:
        // X------f------)[--t--)[ .............Seq1............. X
        //               0      10
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(5`)[`), complementary = false),
          reference
        ),
        // current:
        // X------f------)[--t--)[ .............Seq1............. X
        //               0      10
        // other:
        // X---------t-------)[------------false-----------------X
        //                   5
        // result:
        // X----------t---------)[ .............Seq1............. X
        //                      10
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(5`)[`), complementary = true),
          (true  forAll x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80) ::
          Nil
        ),
        // current:
        // X------f------)[--t--)[ .............Seq1............. X
        //               0      10
        // other:
        // X-t-)[-f-](-t-)[----false---](----------true-----------X
        //    -10   -5   0             15
        //
        // result:
        // X-t-)[-f-](-----t----)[ .............Seq1............. X
        //    -10   -5          10
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("E"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-10`)[`, -5`](`, 10`)[`, 15`](`), complementary = true),
          (true  forAll x <  -10) ::
          (false forAll x >= -10 & x <= -5) ::
          (true  forAll x >  -5  & x <  10) ::
          (false forAll x >=  10 & x <  20) ::
          (true  forAll x >=  20 & x <  30) ::
          (false forAll x >=  30 & x <  40) ::
          (true  forAll x >=  40 & x <= 50) ::
          (false forAll x >   50 & x <= 60) ::
          (true  forAll x >   60 & x <  70) ::
          (false forAll x >=  70 & x <  80) ::
          (true  forAll x >=  80) ::
          Nil
        ),
        // current:
        // X------f------)[--t--)[ .............Seq1............. X
        //               0      10
        // other:
        // X-f-)[-t-](-f-)[-----true---](---------false-----------X
        //    -10   -5   0             15
        //
        // result:
        // X-f-)[-t-](-f-)[--t-)[ .............Seq1............. X
        //    -10   -5   0     10
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("F"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-10`)[`, -5`](`, 10`)[`, 15`](`), complementary = false),
          (false forAll x <  -10) ::
          (true  forAll x >= -10 & x <= -5) ::
          (false forAll x >  -5  & x <   0) ::
          (true  forAll x >=  0  & x <  10) ::
          (false forAll x >=  10 & x <  20) ::
          (true  forAll x >=  20 & x <  30) ::
          (false forAll x >=  30 & x <  40) ::
          (true  forAll x >=  40 & x <= 50) ::
          (false forAll x >   50 & x <= 60) ::
          (true  forAll x >   60 & x <  70) ::
          (false forAll x >=  70 & x <  80) ::
          (true  forAll x >=  80) ::
          Nil
        ),
        // current:
        // X------f------)[--t--)[ .............Seq1............. X
        //               0      10
        // other:
        //     true  true
        // X-f-)|(-f-)|(--f--)[-----true---](---------false-----------X
        //    -10    -5      5             15
        //
        // result:
        //     true  true
        // X-f-)|(-t-)|(f)[--t--)[ .............Seq1............. X
        //    -10    -5  0      10
        SegmentSeqPrependTest.TestCase(
          factoryTuple._1 + Label("G"),
          factoryTuple._2.unsafeBuildAsc(
            ArraySeq(-10`)[`, -10`](`, -5`)[`, -5`](`, 0`)[`, 10`)[`),
            complementary = false
          ),
          (false forAll x <  -10) ::
          (true  forAll x >= -10 & x <= -10) ::
          (false forAll x >  -10 & x <  -5) ::
          (true  forAll x >=  -5 & x <= -5) ::
          (false forAll x >   -5 & x <   0) ::
          (true  forAll x >=   0 & x <  10) ::
          (false forAll x >=  10 & x <  20) ::
          (true  forAll x >=  20 & x <  30) ::
          (false forAll x >=  30 & x <  40) ::
          (true  forAll x >=  40 & x <= 50) ::
          (false forAll x >   50 & x <= 60) ::
          (true  forAll x >   60 & x <  70) ::
          (false forAll x >=  70 & x <  80) ::
          (true  forAll x >=  80) ::
          Nil
        )
      )
    }
  }
  
  override lazy val prependBelowBoundCases: Seq[SegmentSeqPrependTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //  bound
        //    ]
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        // X-----------------------------false----------------------------------X
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          -10 `]`,
          factoryTuple._2.buildUniform(false),
          reference
        ),
        // current:
        //     bound
        //       )
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        // X-----------------------------false----------------------------------X
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A2"),
          0`)`,
          factoryTuple._2.buildUniform(false),
          reference
        ),
        // current:
        //      bound
        //        ]
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        //
        // X--f---](-t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //        0     10     20     30     40     50     60     70     80
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A3"),
          0`]`,
          factoryTuple._2.buildUniform(false),
          (false forAll x <= 0) ::
          (true  forAll x >  0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80) ::
          Nil
        ),
        // current:
        //                                        bound
        //                                          [
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        //                                         true
        // X----------------false------------------)|(--f--](--t--)[--f--)[--t--X
        //                                          50     60     70     80
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A4"),
          50`)`,
          factoryTuple._2.buildUniform(false),
          (false forAll x <  50) ::
          (true  forAll x >= 50 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80) ::
          Nil
        ),
        // current:
        //                                                             bound
        //                                                               )
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        // X-----------------------------false---------------------------)[--t--X
        //                                                               80
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A5"),
          80`)`,
          factoryTuple._2.buildUniform(false),
          (false forAll x <  80) ::
          (true  forAll x >= 80) ::
          Nil
        ),
        // current:
        //                                                                 bound
        //                                                                   )
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        // X-----------------------------false-------------------------------)[tX
        //                                                                   85
        //
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("A6"),
          85`)`,
          factoryTuple._2.buildUniform(false),
          (false forAll x <  85) ::
          (true  forAll x >= 85) ::
          Nil
        ),
        // current:
        //                        bound
        //                          (
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X---------false--------)[-------true-----](-false--](-------true-----X
        //                        25                50        65
        // result:
        // X---------false--------)[-t)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //                        25  30     40     50     60     70     80
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("B1"),
          25`(`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(25`)[`, 50`](`, 65`](`), complementary = false),
          (false forAll x <  25) ::
          (true  forAll x >= 25 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80) ::
          Nil
        ),
        // current:
        //                                        bound
        //                                          ]
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X---------false---------)[------true-----](-false--](-------true-----X
        //                         25               50        65
        // result:
        // X---------false---------)[------true-----](--f--](--t--)[--f--)[--t--X
        //                         25               50     60     70     80
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("B2"),
          50`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(25`)[`, 50`](`, 65`](`), complementary = false),
          (false forAll x <  25) ::
          (true  forAll x >= 25 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80) ::
          Nil
        ),
        // current:
        //                                                  bound
        //                                                    [
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X---------false---------)[------true-----](-false--](-------true-----X
        //                         25               50        65
        // result:
        // X---------false---------)[------true-----](-false-)[-t-)[--f--)[--t--X
        //                         25               50       65   70     80
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("B3"),
          65`[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(25`)[`, 50`](`, 65`](`), complementary = false),
          (false forAll x <  25) ::
          (true  forAll x >= 25 & x <= 50) ::
          (false forAll x >  50 & x <  65) ::
          (true  forAll x >= 65 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80) ::
          Nil
        ),
        // current:
        //
        // bound
        // X
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-f-](--------------------------true---------------------------------X
        //     -5
        //
        // result:
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("C1"),
          ExtendedBound.BelowAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          reference
        ),
        // current:
        //                                                                  bound
        //                                                                      X
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-f-](--------------------------true---------------------------------X
        //     -5
        //
        // result:
        // X-f-](--------------------------true---------------------------------X
        //     -5
        SegmentSeqPrependTest.TestCaseWithBound(
          factoryTuple._1 + Label("C2"),
          ExtendedBound.AboveAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
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
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // other:
        // X--------------------false---------------------X
        //
        // result:
        // X ..Seq1.. )[--------------false---------------X
        //            70
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("A"),
          factoryTuple._2.buildUniform(false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70) ::
          Nil
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // other:
        // X--------------------true----------------------X
        //
        // result:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("B"),
          factoryTuple._2.buildUniform(true),
          reference
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // other:
        // X------true-----)[-----------false-------------X
        //                 75
        // result:
        // X ..Seq1.. )[-------------false----------------X
        //            70
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("C"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(75 `)[`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70) ::
          Nil
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // other:
        // X-----false-----)[------------true-------------X
        //                 75
        // result:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("D"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(75 `)[`), complementary = false),
          reference
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // other:
        //                               true
        // X--false--](--true--)[--false--)|(----false----X
        //           25        80         90
        // result:
        //                               true
        // X ..Seq1.. )[------false-------)|(----false----X
        //            70                  90
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("E"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(25 `](`, 80 `)[`, 90 `)[`, 90 `](`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  90) ::
          (true  forAll x >= 90 & x <= 90) ::
          (false forAll x >  90) ::
          Nil
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // other:
        //                               false
        // X---true--](-false--)[---true--)|(----true-----X
        //           25        80         90
        // result:
        //                               false
        // X ..Seq1.. )[-false-)[---true--)|(----true-----X
        //            70       80         90
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("F"),
          factoryTuple._2.unsafeBuildAsc(ArraySeq(25 `](`, 80 `)[`, 90 `)[`, 90 `](`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80 & x <  90) ::
          (false forAll x >= 90 & x <= 90) ::
          (true  forAll x >  90) ::
          Nil
        ),
        // current:
        // X ..Seq1.. )[-false-)[----------true-----------X
        //            70       80
        // other:
        //                        false false false
        // X----------true---------)|(-t-)|(-t-)|(--true--X
        //                         82    84    86
        // result:
        //                        false false false
        // X ..Seq1.. )[-false-)[--)|(-t-)|(-t-)|(--true--X
        //            70       80  82    84    86
        SegmentSeqAppendTest.TestCase(
          factoryTuple._1 + Label("G"),
          factoryTuple._2.unsafeBuildAsc(
            ArraySeq(82 `)[`, 82 `](`, 84 `)[`, 84 `](`, 86 `)[`, 86 `](`),
            complementary = true
          ),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80 & x <  82) ::
          (false forAll x >= 82 & x <= 82) ::
          (true  forAll x >  82 & x <  84) ::
          (false forAll x >= 84 & x <= 84) ::
          (true  forAll x >  84 & x <  86) ::
          (false forAll x >= 86 & x <= 86) ::
          (true  forAll x >  86) ::
          Nil
        )
      )
    }
  }
  
  override lazy val appendAboveBoundCases: Seq[SegmentSeqAppendTest.TestCaseWithBound[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        //  bound
        //    ]
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        // X-----------------------------false----------------------------------X
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A1"),
          -10`]`,
          factoryTuple._2.buildUniform(false),
          (false forAll x) ::
          Nil
        ),
        // current:
        //     bound
        //       )
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        // X-----------------------------false----------------------------------X
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A2"),
          0`)`,
          factoryTuple._2.buildUniform(false),
          (false forAll x) ::
          Nil
        ),
        // current:
        //      bound
        //        ]
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        //       true
        // X--f--)|(---------------------false----------------------------------X
        //        0
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A3"),
          0`]`,
          factoryTuple._2.buildUniform(false),
          (false forAll x < 0) ::
          (true  forAll x >= 0 & x <= 0) ::
          (false forAll x > 0) ::
          Nil
        ),
        // current:
        //                          bound
        //                            )
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        // X--f--)[--t--)[--f--)[--t--)[-------------------false----------------X
        //       0      10     20     30
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A4"),
          30`)`,
          factoryTuple._2.buildUniform(false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30) ::
          Nil
        ),
        // current:
        //                                                             bound
        //                                                               )
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[----false---X
        //       0      10     20     30     40     50     60     70
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A5"),
          80`)`,
          factoryTuple._2.buildUniform(false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70) ::
          Nil
        ),
        // current:
        //                                                              bound
        //                                                                ]
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-----------------------------false----------------------------------X
        //
        // result:
        //                                                              true
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)|(-f--X
        //       0      10     20     30     40     50     60     70     80
        //
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("A6"),
          80`]`,
          factoryTuple._2.buildUniform(false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80 & x <= 80) ::
          (false forAll x >  80) ::
          Nil
        ),
        // current:
        //         bound
        //           ]
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X---------false---------)[------true-----](-false--](-------true-----X
        //                         25               50        65
        // result:
        // X--f--)[t-](----false---)[------true-----](-false--](-------true-----X
        //       0   5             25               50        65
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("B1"),
          5`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(25`)[`, 50`](`, 65`](`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <= 5) ::
          (false forAll x >  5  & x <  25) ::
          (true  forAll x >= 25 & x <= 50) ::
          (false forAll x >  50 & x <= 65) ::
          (true  forAll x >  65) ::
          Nil
        ),
        // current:
        //                                        bound
        //                                          ]
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X---------false---------)[------true-----](-false--](-------true-----X
        //                         25               50        65
        // result:
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](-false--](-------true-----X
        //       0      10     20     30     40     50        65
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("B2"),
          50`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(25`)[`, 50`](`, 65`](`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 65) ::
          (true  forAll x >  65) ::
          Nil
        ),
        // current:
        //                                                  bound
        //                                                    ]
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X---------false---------)[------true-----](-false--](-------true-----X
        //                         25               50        65
        // result:
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](-------true--------X
        //       0      10     20     30     40     50     60
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("B3"),
          65`]`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(25`)[`, 50`](`, 65`](`), complementary = false),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60) ::
          Nil
        ),
        // current:
        //
        // bound
        // X
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-f-](----------------------------true-------------------------------X
        //     -5
        //
        // result:
        // X-f-](----------------------------true-------------------------------X
        //     -5
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("C1"),
          ExtendedBound.BelowAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          (false forAll x <= -5) ::
          (true  forAll x >  -5) ::
          Nil
        ),
        // current:
        //                                                                  bound
        //                                                                      X
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // other:
        // X-f-](----------------------------true-------------------------------X
        //     -5
        //
        // result:
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        SegmentSeqAppendTest.TestCaseWithBound(
          factoryTuple._1 + Label("C2"),
          ExtendedBound.AboveAll,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(-5`](`), complementary = false),
          reference
        )
      )
    }
  }

  override lazy val sliceCases: Seq[SegmentSeqSliceTest.TestCase[Int, D, Boolean]] = {
    List(
      // current:
      //  bound
      //    ]
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      //
      // takeBelow:
      // X-----------------------------false----------------------------------X
      //
      // takeAbove:
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      SegmentSeqSliceTest.TestCase(
        -10`]`,
        (false forAll x) ::
        Nil,
        reference
      ),
      // current:
      //     bound
      //       )
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      //
      // takeBelow:
      // X-----------------------------false----------------------------------X
      //
      // takeAbove:
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      SegmentSeqSliceTest.TestCase(
        0`)`,
        (false forAll x) ::
        Nil,
        reference
      ),
      // current:
      //         bound
      //           )
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      //
      // takeBelow:
      // X--f--)[-------------------------true--------------------------------X
      //       0
      //
      // takeAbove:
      // X----true----)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      SegmentSeqSliceTest.TestCase(
        5`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0) ::
        Nil,
        (true  forAll x <  10) ::
        (false forAll x >= 10 & x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60 & x <  70) ::
        (false forAll x >= 70 & x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      // current:
      //                   bound
      //                     )
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      //
      // takeBelow:
      // X--f--)[--t--)[-------------------false-----------------------------X
      //       0      10
      //
      // takeAbove:
      // X-------false-------)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //                     20     30     40     50     60     70     80
      SegmentSeqSliceTest.TestCase(
        20`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <  10) ::
        (false forAll x >= 10 ) ::
        Nil,
        (false forAll x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60 & x <  70) ::
        (false forAll x >= 70 & x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      // current:
      //                                                bound
      //                                                  (
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      //
      // takeBelow:
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--------true-------X
      //       0      10     20     30     40     50     60
      //
      // takeAbove:
      // X------------------------true--------------------------)[--f--)[--t--X
      //                                                        70     80
      SegmentSeqSliceTest.TestCase(
        60`(`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <  10) ::
        (false forAll x >= 10 & x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60) ::
        Nil,
        (true  forAll x <  70) ::
        (false forAll x >= 70 & x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      // current:
      //                                                            bound
      //                                                               )
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      //
      // takeBelow:
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[---false----X
      //       0      10     20     30     40     50     60     70
      //
      // takeAbove:
      // X--------------------------false------------------------------)[--t--X
      //                                                               80
      SegmentSeqSliceTest.TestCase(
        80`)`,
        (false forAll x <  0) ::
        (true  forAll x >= 0  & x <  10) ::
        (false forAll x >= 10 & x <  20) ::
        (true  forAll x >= 20 & x <  30) ::
        (false forAll x >= 30 & x <  40) ::
        (true  forAll x >= 40 & x <= 50) ::
        (false forAll x >  50 & x <= 60) ::
        (true  forAll x >  60 & x <  70) ::
        (false forAll x >= 70) ::
        Nil,
        (false forAll x <  80) ::
        (true  forAll x >= 80) ::
        Nil
      ),
      // current:
      //                                                              bound
      //                                                                [
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      //
      // takeBelow:
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      //
      // takeAbove:
      // X-----------------------------true-----------------------------------X
      //                                                               
      SegmentSeqSliceTest.TestCase(
        80`[`,
        reference,
        (true  forAll x) ::
        Nil
      ),
      // current:
      //
      // bound
      // X
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      //
      // takeBelow:
      // X-----------------------------false----------------------------------X
      //
      // takeAbove:
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      SegmentSeqSliceTest.TestCase(
        ExtendedBound.BelowAll,
        (false  forAll x) :: Nil,
        reference
      ),
      // current:
      //                                                                  bound
      //                                                                      X
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      //
      // takeBelow:
      // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
      //       0      10     20     30     40     50     60     70     80
      //
      // takeAbove:
      // X-----------------------------true-----------------------------------X
      SegmentSeqSliceTest.TestCase(
        ExtendedBound.AboveAll,
        reference,
        (true  forAll x) :: Nil
      )
    )
  }

  override lazy val patchCases: Seq[SegmentPatchTest.TestCase[Int, D, Boolean]] = {
    SegmentSeqFactories.getOrderedSetFactories.flatMap { factoryTuple =>
      List(
        // current:
        // patched segment
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // patch:
        // X----------------------------false-----------------------------------X
        //
        // result:
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        SegmentPatchTest.TestCase(
          factoryTuple._1 + Label("A1"),
          -5 `[`,
          factoryTuple._2.buildUniform(false),
          reference
        ),
        // current:
        // patched segment
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // patch:
        // X-----------------------------true-----------------------------------X
        //
        // result:
        // X----true----)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //              10     20     30     40     50     60     70     80
        SegmentPatchTest.TestCase(
          factoryTuple._1 + Label("A2"),
          -5 `[`,
          factoryTuple._2.buildUniform(true),
          (true  forAll x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80) ::
          Nil
        ),
        // current:
        // patched segment
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // patch:
        // X-------------true-------------](-----------------false--------------X
        //                                35
        // result:
        // X--f--)[--t--)[--f--)[----t----](f)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20         35 40     50     60     70     80
        SegmentPatchTest.TestCase(
          factoryTuple._1 + Label("B1"),
          35 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(35 `](`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <= 35) ::
          (false forAll x >  35 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70 & x <  80) ::
          (true  forAll x >= 80) ::
          Nil
        ),
        // current:
        // patched segment
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // patch:
        // X-----------------------------true----------------------------)[--f--X
        //                                                               80
        // result:
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[---false----X
        //       0      10     20     30     40     50     60     70
        SegmentPatchTest.TestCase(
          factoryTuple._1 + Label("C1"),
          85 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(80 `)[`), complementary = true),
          (false forAll x <  0) ::
          (true  forAll x >= 0  & x <  10) ::
          (false forAll x >= 10 & x <  20) ::
          (true  forAll x >= 20 & x <  30) ::
          (false forAll x >= 30 & x <  40) ::
          (true  forAll x >= 40 & x <= 50) ::
          (false forAll x >  50 & x <= 60) ::
          (true  forAll x >  60 & x <  70) ::
          (false forAll x >= 70) ::
          Nil
        ),
        // current:
        // patched segment
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        //
        // patch:
        // X-----------------------------false---------------------------)[--t--X
        //                                                               80
        // result:
        // X--f--)[--t--)[--f--)[--t--)[--f--)[--t--](--f--](--t--)[--f--)[--t--X
        //       0      10     20     30     40     50     60     70     80
        SegmentPatchTest.TestCase(
          factoryTuple._1 + Label("C2"),
          85 `[`,
          factoryTuple._2.unsafeBuildAsc(ArraySeq(80 `)[`), complementary = false),
          reference
        )
      )
    }
  }
}
