package ordset.test.core.specs.segmentSeq

import ordset.core.set.OrderedSet
import ordset.core.domain.Domain
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import ordset.test.core.behaviors.segmentSeq.{InspectionBehaviors, NavigationBehaviors, TransformationBehaviors}
import ordset.test.core.samples.segmentSeq.arrayOrderedSet._

@RunWith(classOf[JUnitRunner])
class ArrayOrderedSetSpec extends AnyFunSpec
  with InspectionBehaviors[Int, Domain.UnboundedContinuous[Int], Boolean]
  with NavigationBehaviors[Int, Domain.UnboundedContinuous[Int], Boolean]
  with TransformationBehaviors[Int, Domain.UnboundedContinuous[Int], Boolean] {

  import ordset.core.instances.boolean._
  import ordset.core.instances.int._
  import ordset.test.core.TestRngUtil.Implicits._

  type Dom = Domain.UnboundedContinuous[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val testSuite = List(
    new EmptySetSample1[Dom],
    new UniversalSetSample1[Dom],
    new SingleBoundedSetSample1[Dom],
    new MultiBoundedSetSample1[Dom],
    new MultiBoundedSetSample3[Dom],
    new DegenerateSetSample1[Dom]
  )

  describe("Array based ordered set inspection operations:") {

    it should behave like segmentsHaveNavigationIndicators(testSuite)
    
    it should behave like segmentsSupportContains(testSuite)

    it should behave like segmentsCanRestrictBound(testSuite)

    it should behave like supportReturnValueForBound(testSuite)
  }
  
  describe("Array based ordered set navigation operations:") {

    it should behave like segmentsSupportMovePrevAndNext(testSuite)

    it should behave like supportMoveToBound(testSuite)

    it should behave like supportMoveToFirstAndLast(testSuite)
  }

  describe("Array based ordered set transformation operations:") {

    it should behave like segmentSeqCanBePrepended(testSuite)

    it should behave like segmentSeqCanBePrependedBelowBound(testSuite)

    it should behave like segmentSeqCanBeAppended(testSuite)

    it should behave like segmentSeqCanBeAppendedAboveBound(testSuite)

    it should behave like segmentSeqCanBeSliced(testSuite)

    it should behave like segmentCanBePatched(testSuite)
  }
}