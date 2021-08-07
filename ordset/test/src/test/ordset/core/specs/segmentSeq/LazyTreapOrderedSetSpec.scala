package test.ordset.core.specs.segmentSeq

import ordset.core.domain.Domain
import ordset.core.set.OrderedSet
import org.junit.runner.RunWith
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.junit.JUnitRunner
import test.ordset.core.behaviors.lazyTreapSeq.LazyTreapSeqBehaviours
import test.ordset.core.domain.BoundSelector
import test.ordset.core.samples.segmentSeq.LazyTreapSeqUtil
import test.ordset.core.samples.segmentSeq.lazyTreapOrderedSet._
import test.ordset.core.{RandomUtil, SegmentSeqAssertions, TestRngUtil}

@RunWith(classOf[JUnitRunner])
class LazyTreapOrderedSetSpec extends AnyFunSpec
  with LazyTreapSeqBehaviours[Int, Domain[Int], Boolean] {

  import ordset.core.instances.boolean._
  import ordset.core.instances.int._
  import test.ordset.core.TestRngUtil.Implicits._

  type Dom = Domain[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val cacheSuite = List(
    new EmptySetSampleLT1[Dom],
    new SingleBounedSetSampleLT1[Dom],
    new MultiBoundedSetSampleLT1[Dom],
    new MultiBoundedSetSampleLT2[Dom],
  )

  private val testSuite = List(
    cacheSuite,
    (1 to 10).flatMap { seed =>
      List(
        new EmptySetSample1[Dom](seed),
        new SingleBoundedSetSample1[Dom](seed)
      )
    },
    (1 to 100).flatMap { seed =>
      List(
        new MultiBoundedSetSample3[Dom](seed)
      )
    }
  ).flatten

  describe("Lazy treap ordered set specific operations:") {

    it should behave like sequenceProperlyCacheLazyValues(cacheSuite)

    it should behave like sequenceHasValidStateAfterSequentialRandomAccess(testSuite)

    it should behave like sequenceHasValidStateAfterConcurrentRandomAccess(testSuite)
  }
}