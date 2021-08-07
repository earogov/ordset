package test.ordset.core.behaviors.lazyTreapSeq.singleBoundedSet

import ordset.core.AbstractLazyTreapSegmentSeq.EagerValue
import ordset.core.{ExtendedBound, IntervalRelation}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.core.syntax.BoundSyntax._
import ordset.core.syntax.SetBuilderNotation._
import ordset.random.RngManager
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.lazyTreapSeq.LazyTreapSeqCacheTest
import test.ordset.core.behaviors.zippedSeq.OriginalSeqPatchTest
import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

trait SampleLT1[D <: Domain[Int]]
  extends LazyTreapSeqCacheTest[Int, D, Boolean] {
  self: LazyTreapSeqSample.Fixed[Int, D, Boolean] =>

  override val sample: String = "LT1"

  override val reference: Seq[IntervalRelation[Int, D, Boolean]] = List(
    false forAll (x < 0),
    true forAll (x >= 0)
  )

  override lazy val lazyCacheCases: Iterable[LazyTreapSeqCacheTest.TestPackage[Int, D, Boolean]] = List(
    LazyTreapSeqCacheTest.TestPackage(
      Set(Label("A")),
      List(
        LazyTreapSeqCacheTest.ValueTestCase(
          -5 `)`,
          false,
          List(
            someLazyZvalue forAll (x < -10),
            (false, EagerValue.unstable[Int, D, Boolean]) forAll (x >= -10 & x < 0),
            (true, EagerValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x <= 10),
            someLazyZvalue forAll (x > 10)
          )
        ),
        LazyTreapSeqCacheTest.SegmentTestCase(
          -15 `)`,
          false forAll (x < 0),
          List(
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x < 0),
            (true, EagerValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x <= 10),
            someLazyZvalue forAll (x > 10)
          )
        )
      )
    ),
    LazyTreapSeqCacheTest.TestPackage(
      Set(Label("B")),
      List(
        LazyTreapSeqCacheTest.ValueTestCase(
          -15 `)`,
          false,
          List(
            (false, EagerValue.unstable[Int, D, Boolean]) forAll (x < -10),
            someLazyZvalue forAll (x >= -10 & x <= 10),
            someLazyZvalue forAll (x > 10)
          )
        ),
        LazyTreapSeqCacheTest.ValueTestCase(
          5 `)`,
          true,
          List(
            (false, EagerValue.stable[Int, D, Boolean]) forAll (x < 0),
            (true, EagerValue.unstable[Int, D, Boolean]) forAll (x >= 0 & x <= 10),
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
}
