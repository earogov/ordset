package ordset.test.core.specs.segmentSeq

import ordset.core.domain.Domain
import ordset.core.set.OrderedSet
import org.junit.runner.RunWith
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.junit.JUnitRunner
import ordset.test.core.behaviors.lazyTreapSeq.LazyTreapSeqBehaviors
import ordset.test.core.behaviors.segmentSeq.{InspectionBehaviors, NavigationBehaviors, TransformationBehaviors}
import ordset.test.core.implementations.domain.BoundSelector
import ordset.test.core.implementations.segmentSeq.lazyTreap.LazyTreapSeqUtil
import ordset.test.core.samples.segmentSeq.lazyTreapOrderedSet.*
import ordset.test.core.{RandomUtil, SegmentSeqAssertions, TestRngUtil}

@RunWith(classOf[JUnitRunner])
class LazyTreapOrderedSetSpec extends AnyFunSpec
  with LazyTreapSeqBehaviors[Int, Domain.UnboundedContinuous[Int], Boolean]
  with InspectionBehaviors[Int, Domain.UnboundedContinuous[Int], Boolean]
  with NavigationBehaviors[Int, Domain.UnboundedContinuous[Int], Boolean]
  with TransformationBehaviors[Int, Domain.UnboundedContinuous[Int], Boolean]{

  import ordset.core.instances.boolean._
  import ordset.core.instances.int._
  import ordset.test.core.TestRngUtil.Implicits._

  type Dom = Domain.UnboundedContinuous[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val cacheSuite = List(
    new EmptySetSampleLT1[Dom](false),
    new SingleBoundedSetSampleLT1[Dom](false),
    new MultiBoundedSetSampleLT1[Dom](false),
    new MultiBoundedSetSampleLT2[Dom](false),
  )

  private val accessSuite = List(
    cacheSuite,
    getRandomSamples(false)
  ).flatten

  private val defaultSuite = getRandomSamples(true)

  private def getRandomSamples(shuffled: Boolean) = List(
    (1 to 3).flatMap { seed =>
      List(
        new EmptySetSample1[Dom](seed, shuffled),
        new SingleBoundedSetSample1[Dom](seed, shuffled),
        new DegenerateSetSample1[Dom](seed, shuffled)
      )
    },
    (1 to 10).flatMap { seed =>
      List(
        new MultiBoundedSetSample3[Dom](seed, shuffled)
      )
    }
  ).flatten

  describe("Lazy treap ordered set inspection operations:") {

    it should behave like segmentsHaveNavigationIndicators(defaultSuite)

    it should behave like segmentsSupportContains(defaultSuite)

    it should behave like segmentsCanRestrictBound(defaultSuite)

    it should behave like supportReturnValueForBound(defaultSuite)
  }

  describe("Lazy treap ordered set navigation operations:") {

    it should behave like segmentsSupportMovePrevAndNext(defaultSuite)

    it should behave like supportMoveToBound(defaultSuite)

    it should behave like supportMoveToFirstAndLast(defaultSuite)
  }

  describe("Lazy treap ordered set transformation operations:") {

    it should behave like segmentSeqCanBePrepended(defaultSuite)

    it should behave like segmentSeqCanBePrependedBelowBound(defaultSuite)

    it should behave like segmentSeqCanBeAppended(defaultSuite)

    it should behave like segmentSeqCanBeAppendedAboveBound(defaultSuite)

    it should behave like segmentSeqCanBeSliced(defaultSuite)

    it should behave like segmentCanBePatched(defaultSuite)
  }

  describe("Lazy treap ordered set specific operations:") {

    it should behave like sequenceProperlyCacheLazyValues(cacheSuite)

    it should behave like sequenceHasValidStateAfterSequentialRandomAccess(accessSuite)

    it should behave like sequenceHasValidStateAfterConcurrentRandomAccess(accessSuite)
  }
}