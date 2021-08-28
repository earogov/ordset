package test.ordset.core.specs.segmentSeq

import ordset.core.set.OrderedSet
import ordset.core.domain.Domain
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.behaviors.segmentSeq.{InspectionBehaviors, NavigationBehaviors, TransformationBehaviors}
import test.ordset.core.behaviors.zippedSeq.ZippedSeqBehaviours
import test.ordset.core.samples.segmentSeq.zippedOrderedSet._

@RunWith(classOf[JUnitRunner])
class ZippedOrderedSetSpec extends AnyFunSpec
  with InspectionBehaviors[Int, Domain[Int], Boolean]
  with NavigationBehaviors[Int, Domain[Int], Boolean]
  with TransformationBehaviors[Int, Domain[Int], Boolean]
  with ZippedSeqBehaviours[Int, Domain[Int], Boolean, Boolean, Boolean] {

  import ordset.core.instances.boolean._
  import ordset.core.instances.int._
  import test.ordset.core.TestRngUtil.Implicits._

  type Dom = Domain[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val testSuite = List(
    new EmptySetSample1[Dom],
    new UniversalSetSample1[Dom],
    new SingleBoundedSetSample1[Dom],
    new MultiBoundedSetSample2[Dom],
    new DegenerateSetSample1[Dom]
  )

  private val transformSuite = List(
//    new EmptySetSample1[Dom],
//    new UniversalSetSample1[Dom],
//    new SingleBoundedSetSample1[Dom],
//    new MultiBoundedSetSample2[Dom],
//    new DegenerateSetSample1[Dom]
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
    
//    it should behave like segmentSeqCanBeSliced(transformSuite)
  }

  describe("Zipped ordered set specific operations:") {

    it should behave like segmentCanPatchOriginalSeq(zippedSuit)
  }
}