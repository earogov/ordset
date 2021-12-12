package ordset.test.core.specs.segmentSeq

import ordset.core.set.OrderedSet
import ordset.core.domain.Domain
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import ordset.test.core.behaviors.segmentSeq.{InspectionBehaviors, NavigationBehaviors, TransformationBehaviors}
import ordset.test.core.behaviors.zippedSeq.ZippedSeqBehaviors
import ordset.test.core.samples.segmentSeq.zippedOrderedSet._

@RunWith(classOf[JUnitRunner])
class ZippedOrderedSetSpec extends AnyFunSpec
  with InspectionBehaviors[Int, Domain.UnboundedContinuous[Int], Boolean]
  with NavigationBehaviors[Int, Domain.UnboundedContinuous[Int], Boolean]
  with TransformationBehaviors[Int, Domain.UnboundedContinuous[Int], Boolean]
  with ZippedSeqBehaviors[Int, Domain.UnboundedContinuous[Int], Boolean, Boolean, Boolean] {

  import ordset.instances.boolean._
  import ordset.instances.int._
  import ordset.test.core.TestRngUtil.Implicits._

  type Dom = Domain.UnboundedContinuous[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val testSuite = List(
    new EmptySetSample1[Dom],
    new UniversalSetSample1[Dom],
    new SingleBoundedSetSample1[Dom],
    new MultiBoundedSetSample2[Dom],
    new DegenerateSetSample1[Dom]
  )

  private val transformSuite = List(
    new EmptySetSample1[Dom],
    new UniversalSetSample1[Dom],
    new SingleBoundedSetSample1[Dom],
    new DegenerateSetSample1[Dom]
  )

  private val zippedSuit = List(
    new EmptySetSampleZ1[Dom],
    new UniversalSetSampleZ1[Dom],
    new MultiBoundedSetSampleZ1[Dom]
  )

  describe("Zipped ordered set inspection operations:") {

    it should behave like segmentsHaveNavigationIndicators(testSuite)

    it should behave like segmentsSupportContains(testSuite)

    it should behave like segmentsCanRestrictBound(testSuite)

    it should behave like supportReturnValueForBound(testSuite)
  }

  describe("Zipped ordered set navigation operations:") {

    it should behave like segmentsSupportMovePrevAndNext(testSuite)

    it should behave like supportMoveToBound(testSuite)

    it should behave like supportMoveToFirstAndLast(testSuite)
  }

  describe("Zipped ordered set transformation operations:") {
    
    it should behave like segmentSeqCanBePrepended(transformSuite)

    it should behave like segmentSeqCanBePrependedBelowBound(transformSuite)

    it should behave like segmentSeqCanBeAppended(transformSuite)

    it should behave like segmentSeqCanBeAppendedAboveBound(transformSuite)

    it should behave like segmentSeqCanBeSliced(transformSuite)

    it should behave like segmentCanBePatched(transformSuite)
  }

  describe("Zipped ordered set specific operations:") {

    it should behave like segmentCanPatchOriginalSeq(zippedSuit)
  }
}