package ordset.test.core.behaviors.lazyTreapSeq.set.singleBoundedSet

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

trait SampleLT1[D[X] <: Domain[X]]
  extends LazyTreapSeqCacheTest[Int, D, Boolean]
  with LazyTreapSeqMultipleTakeTest[Int, D, Boolean] {
  self: LazyTreapSeqSample.Fixed[Int, D, Boolean] =>

  override val sample: String = "LT1"

  override val reference: Seq[IntervalRelation[Int, D, Boolean]] = List(
    false forAll (x < 0),
    true forAll (x >= 0)
  )

  override lazy val lazyCacheCases: Iterable[LazyTreapSeqCacheTest.TestPackage[Int, D, Boolean]] = List(
    LazyTreapSeqCacheTest.TestPackage(
      Set(label("A")),
      List(
        LazyTreapSeqCacheTest.ValueTestCase(
          -5 `)`,
          false,
          List(
            someLazyZvalue forAll (x < -10),
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x >= -10 & x < 0),
            (true, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x <= 10),
            someLazyZvalue forAll (x > 10)
          )
        ),
        LazyTreapSeqCacheTest.SegmentTestCase(
          -15 `)`,
          false forAll (x < 0),
          List(
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x < 0),
            (true, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x <= 10),
            someLazyZvalue forAll (x > 10)
          )
        )
      )
    ),
    LazyTreapSeqCacheTest.TestPackage(
      Set(label("B")),
      List(
        LazyTreapSeqCacheTest.ValueTestCase(
          -15 `)`,
          false,
          List(
            (false, StrictValue.unstable[Int, D, Boolean]) forAll (x < -10),
            someLazyZvalue forAll (x >= -10 & x <= 10),
            someLazyZvalue forAll (x > 10)
          )
        ),
        LazyTreapSeqCacheTest.ValueTestCase(
          5 `)`,
          true,
          List(
            (false, StrictValue.stable[Int, D, Boolean]) forAll (x < 0),
            (true, StrictValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x <= 10),
            someLazyZvalue forAll (x > 10)
          )
        ),
        LazyTreapSeqCacheTest.ValueTestCase(
          15 `)`,
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
        LazyTreapSeqMultipleTakeTest.TakeAboveCommand(-1`[`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            false forAll (x < 0),
            true forAll (x >= 0)
          )
        ),
        LazyTreapSeqMultipleTakeTest.TakeBelowCommand(1`)`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            false forAll (x < 0),
            true forAll (x >= 0)
          )
        ),
        LazyTreapSeqMultipleTakeTest.TakeBelowCommand(0`)`),
        LazyTreapSeqMultipleTakeTest.Validation(
          List(
            false forAll x
          )
        )
      )
    )
  )
}
