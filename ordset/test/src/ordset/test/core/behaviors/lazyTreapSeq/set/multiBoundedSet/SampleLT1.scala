package ordset.test.core.behaviors.lazyTreapSeq.set.multiBoundedSet

import ordset.core.segmentSeq.internal.lazySeq.ControlValue.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.core.ExtendedBound
import ordset.core.interval.IntervalRelation
import ordset.random.RngManager
import ordset.test.Label
import ordset.test.Label.*
import ordset.test.core.behaviors.lazyTreapSeq.{LazyTreapSeqCacheTest, LazyTreapSeqMultipleTakeTest}
import ordset.test.core.behaviors.zippedSeq.OriginalSeqPatchTest
import ordset.test.core.samples.segmentSeq.LazyTreapSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait SampleLT1[D[X] <: Domain[X]]
  extends LazyTreapSeqCacheTest[Int, D, Boolean]
  with LazyTreapSeqMultipleTakeTest[Int, D, Boolean] {
  self: LazyTreapSeqSample.Fixed[Int, D, Boolean] =>

  override val sample: String = "LT1"

  override val reference: Seq[IntervalRelation[Int, D, Boolean]] = List(
    true forAll (x < -10),
    false forAll (x >= -10 & x <= -5),
    true forAll (x > -5 & x < 0),
    false forAll (x >= 0 & x < 2),
    true forAll (x >= 2 & x <= 8),
    false forAll (x > 8 & x < 15),
    true forAll (x >= 15 & x <= 20),
    false forAll (x > 20)
  )

  override lazy val lazyCacheCases: Iterable[LazyTreapSeqCacheTest.TestPackage[Int, D, Boolean]] = List(
    LazyTreapSeqCacheTest.TestPackage(
      Set(label("A")),
      List(
        LazyTreapSeqCacheTest.SegmentTestCase(
          5`(`,
          true forAll (x >= 2 & x <= 8),
          List(
            someLazyZvalue forAll (x < 0),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x < 2),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 2 & x <= 8),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x > 8 & x <= 10),
            someLazyZvalue forAll (x > 10)
          )
        ),
        LazyTreapSeqCacheTest.SegmentTestCase(
          17`(`,
          true forAll (x >= 15 & x <= 20),
          List(
            someLazyZvalue forAll (x < 0),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x < 2),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 2 & x <= 8),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 8 & x < 15),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 20)
          )
        ),
        LazyTreapSeqCacheTest.SegmentTestCase(
          -5`(`,
          true forAll (x > -5 & x < 0),
          zippedReference
        )
      )
    ),
    LazyTreapSeqCacheTest.TestPackage(
      Set(label("B")),
      List(
        LazyTreapSeqCacheTest.SegmentTestCase(
          12`(`,
          false forAll (x > 8 & x < 15),
          List(
            someLazyZvalue forAll (x < 0),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x < 2),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 2 & x <= 8),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 8 & x < 15),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 20)
          )
        ),
        LazyTreapSeqCacheTest.SegmentTestCase(
          -30`(`,
          true forAll (x < -10),
          zippedReference
        )
      )
    ),
    LazyTreapSeqCacheTest.TestPackage(
      Set(label("C")),
      List(
        LazyTreapSeqCacheTest.ValueTestCase(
          1`(`,
          false,
          List(
            someLazyZvalue forAll (x < 0),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x < 2),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 2 & x <= 8),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x > 8 & x <= 10),
            someLazyZvalue forAll (x > 10),
          )
        ),
        LazyTreapSeqCacheTest.ValueTestCase(
          11`(`,
          false,
          List(
            someLazyZvalue forAll (x < 0),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x < 2),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 2 & x <= 8),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 8 & x < 15),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 20)
          )
        ),
        LazyTreapSeqCacheTest.ValueTestCase(
          -11`(`,
          true,
          zippedReference
        )
      )
    )
  )

  override lazy val multipleTakeCases: Iterable[LazyTreapSeqMultipleTakeTest.TestPackage[Int, D, Boolean]] = List(
    LazyTreapSeqMultipleTakeTest.TestPackage(
      Set(label("A")),
      List(
        LazyTreapSeqMultipleTakeTest.TakeAboveCommand(1`(`),
        LazyTreapSeqMultipleTakeTest.TakeAboveCommand(1`(`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            false forAll (x < 2),
            true forAll (x >= 2 & x <= 8),
            false forAll (x > 8 & x < 15),
            true forAll (x >= 15 & x <= 20),
            false forAll (x > 20)
          )
        ),
        LazyTreapSeqMultipleTakeTest.TakeBelowCommand(8`(`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            false forAll (x < 2),
            true forAll (x >= 2 & x <= 8),
            false forAll (x > 8)
          )
        ),
        LazyTreapSeqMultipleTakeTest.TakeBelowCommand(100`(`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            false forAll (x < 2),
            true forAll (x >= 2 & x <= 8),
            false forAll (x > 8)
          )
        ),
        LazyTreapSeqMultipleTakeTest.TakeBelowCommand(0`(`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            false forAll x,
          )
        ),
        LazyTreapSeqMultipleTakeTest.TakeAboveCommand(-1`(`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            false forAll x,
          )
        )
      )
    ),
    LazyTreapSeqMultipleTakeTest.TestPackage(
      Set(label("B")),
      // Check that there is no stack overflow.
      (-10000 to 0)
        .map { i => LazyTreapSeqMultipleTakeTest.TakeAboveCommand(i`[`) }
        .appended(
          LazyTreapSeqMultipleTakeTest.Validation(
            List(
              false forAll (x < 2),
              true forAll (x >= 2 & x <= 8),
              false forAll (x > 8 & x < 15),
              true forAll (x >= 15 & x <= 20),
              false forAll (x > 20)
            )
          )
        )
    )
  )
}
