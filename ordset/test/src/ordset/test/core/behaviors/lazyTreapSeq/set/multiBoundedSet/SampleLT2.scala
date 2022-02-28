package ordset.test.core.behaviors.lazyTreapSeq.set.multiBoundedSet

import ordset.core.segmentSeq.internal.lazySeq.ControlValue.*
import ordset.core.ExtendedBound
import ordset.core.interval.IntervalRelation
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.random.RngManager
import ordset.test.Label
import ordset.test.Label.*
import ordset.test.core.behaviors.lazyTreapSeq.{LazyTreapSeqCacheTest, LazyTreapSeqMultipleTakeTest}
import ordset.test.core.behaviors.zippedSeq.OriginalSeqPatchTest
import ordset.test.core.samples.segmentSeq.LazyTreapSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait SampleLT2[D[X] <: Domain[X]]
  extends LazyTreapSeqCacheTest[Int, D, Boolean]
  with LazyTreapSeqMultipleTakeTest[Int, D, Boolean] {
  self: LazyTreapSeqSample.Fixed[Int, D, Boolean] =>

  override val sample: String = "LT2"

  override val reference: Seq[IntervalRelation[Int, D, Boolean]] = List(
    false forAll (x < -10),
    true forAll (x >= -10 & x <= -5),
    false forAll (x > -5 & x < 15),
    true forAll (x >= 15 & x <= 20),
    false forAll (x > 20 & x < 25),
    true forAll (x >= 25 & x < 30),
    false forAll (x >= 30)
  )

  override lazy val lazyCacheCases: Iterable[LazyTreapSeqCacheTest.TestPackage[Int, D, Boolean]] = List(
    LazyTreapSeqCacheTest.TestPackage(
      Set(label("A")),
      List(
        LazyTreapSeqCacheTest.SegmentTestCase(
          5 `(`,
          false forAll (x > -5 & x < 15),
          List(
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x < -10),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= -10 & x <= -5),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > -5 & x < 15),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 20 & x < 25),
            (true, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 25 & x < 30),
            someLazyZvalue forAll (x >= 30)
          )
        ),
        LazyTreapSeqCacheTest.SegmentTestCase(
          27 `(`,
          true forAll (x >= 25 & x < 30),
          zippedReference
        )
      )
    ),
    LazyTreapSeqCacheTest.TestPackage(
      Set(label("B")),
      List(
        LazyTreapSeqCacheTest.SegmentTestCase(
          23 `(`,
          false forAll (x > 20 & x < 25),
          List(
            someLazyZvalue forAll (x < 0),
            someLazyZvalue forAll (x >= 0 & x <= 10),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x > 10 & x < 15),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 20 & x < 25),
            (true, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 25 & x < 30),
            someLazyZvalue forAll (x >= 30)
          )
        ),
        LazyTreapSeqCacheTest.SegmentTestCase(
          -2 `(`,
          false forAll (x > -5 & x < 15),
          List(
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x < -10),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= -10 & x <= -5),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > -5 & x < 15),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 20 & x < 25),
            (true, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 25 & x < 30),
            someLazyZvalue forAll (x >= 30)
          )
        ),
        LazyTreapSeqCacheTest.SegmentTestCase(
          40 `(`,
          false forAll (x >= 30),
          zippedReference
        )
      )
    ),
    LazyTreapSeqCacheTest.TestPackage(
      Set(label("C")),
      List(
        LazyTreapSeqCacheTest.ValueTestCase(
          5 `(`,
          false,
          List(
            someLazyZvalue forAll (x < 0),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x <= 10),
            someLazyZvalue forAll (x > 10 & x < 30),
            someLazyZvalue forAll (x >= 30)
          )
        ),
        LazyTreapSeqCacheTest.ValueTestCase(
          11 `(`,
          false,
          List(
            someLazyZvalue forAll (x < 0),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x < 15),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 20 & x < 25),
            (true, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 25 & x < 30),
            someLazyZvalue forAll (x >= 30)
          )
        ),
        LazyTreapSeqCacheTest.ValueTestCase(
          -11 `(`,
          false,
          List(
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x < -10),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= -10 & x <= -5),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > -5 & x < 15),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 20 & x < 25),
            (true, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 25 & x < 30),
            someLazyZvalue forAll (x >= 30)
          )
        )
      )
    ),
    LazyTreapSeqCacheTest.TestPackage(
      Set(label("D")),
      List(
        LazyTreapSeqCacheTest.ValueTestCase(
          35 `(`,
          false,
          List(
            someLazyZvalue forAll (x < 0),
            someLazyZvalue forAll (x >= 0 & x <= 10),
            someLazyZvalue forAll (x > 10 & x < 30),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 30),
          )
        ),
        LazyTreapSeqCacheTest.ValueTestCase(
          29 `(`,
          true,
          List(
            someLazyZvalue forAll (x < 0),
            someLazyZvalue forAll (x >= 0 & x <= 10),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x > 10 & x < 15),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 15 & x <= 20),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x > 20 & x < 25),
            (true, StrictValue.stable[Int, D, Boolean]) forAll (x >= 25 & x < 30),
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x >= 30),
          )
        )
      )
    )
  )

  override lazy val multipleTakeCases: Iterable[LazyTreapSeqMultipleTakeTest.TestPackage[Int, D, Boolean]] = List(
    LazyTreapSeqMultipleTakeTest.TestPackage(
      Set(label("A")),
      List(
        LazyTreapSeqMultipleTakeTest.TakeAboveCommand(15`[`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            true forAll (x <= 20),
            false forAll (x > 20 & x < 25),
            true forAll (x >= 25 & x < 30),
            false forAll (x >= 30)
          )
        ),
        LazyTreapSeqMultipleTakeTest.TakeBelowCommand(25`)`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            true forAll (x <= 20),
            false forAll (x > 20)
          )
        ),
        LazyTreapSeqMultipleTakeTest.TakeAboveCommand(20`(`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            false forAll x
          )
        ),
        LazyTreapSeqMultipleTakeTest.TakeBelowCommand(25`)`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            false forAll x
          )
        )
      )
    ),
    LazyTreapSeqMultipleTakeTest.TestPackage(
      Set(label("B")),
      List(
        LazyTreapSeqMultipleTakeTest.TakeAboveCommand(30`(`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            false forAll x
          )
        )
      )
    ),
    LazyTreapSeqMultipleTakeTest.TestPackage(
      Set(label("C")),
      List(
        LazyTreapSeqMultipleTakeTest.TakeAboveCommand(15`[`),
        LazyTreapSeqMultipleTakeTest.TakeBelowCommand(10`)`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            true forAll x
          )
        )
      )
    ),
    LazyTreapSeqMultipleTakeTest.TestPackage(
      Set(label("D")),
      List(
        LazyTreapSeqMultipleTakeTest.TakeAboveCommand(15`[`),
        LazyTreapSeqMultipleTakeTest.TakeBelowCommand(15`[`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            true forAll x
          )
        )
      )
    ),
    LazyTreapSeqMultipleTakeTest.TestPackage(
      Set(label("E")),
      List(
        LazyTreapSeqMultipleTakeTest.TakeBelowCommand(-7`[`),
        LazyTreapSeqMultipleTakeTest.TakeAboveCommand(0`[`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            true forAll x
          )
        )
      )
    )
  )
}
