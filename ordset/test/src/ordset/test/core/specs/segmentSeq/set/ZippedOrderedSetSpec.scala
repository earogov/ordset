package ordset.test.core.specs.segmentSeq.set

import ordset.core.segmentSeq.set.OrderedSet
import ordset.core.domain.Domain
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import ordset.test.core.behaviors.segmentSeq.{InspectionBehaviors, NavigationBehaviors, TransformationBehaviors}
import ordset.test.core.behaviors.segmentSeq.LazyEvalProperties
import ordset.test.core.behaviors.zippedSeq.ZippedSeqBehaviors
import ordset.test.core.samples.segmentSeq.set.zippedOrderedSet._

@RunWith(classOf[JUnitRunner])
class ZippedOrderedSetSpec extends AnyFunSpec
  with InspectionBehaviors[Int, Domain.ContinuousUnbounded, Boolean]
  with NavigationBehaviors[Int, Domain.ContinuousUnbounded, Boolean]
  with TransformationBehaviors[Int, Domain.ContinuousUnbounded, Boolean]
  with ZippedSeqBehaviors[Int, Domain.ContinuousUnbounded, Boolean, Boolean, Boolean]
  with LazyEvalProperties[Int, Domain.ContinuousUnbounded, Boolean] {

  import ordset.givens.boolean._
  import ordset.givens.int._
  import ordset.test.core.TestRngUtil.Givens._

  type Dom[X] = Domain.ContinuousUnbounded[X]
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

    it should behave like segmentSeqCanBeConvertedIntoStrictSequence(transformSuite)
  }

  describe("Zipped ordered set specific operations:") {

    it should behave like segmentCanPatchOriginalSeq(zippedSuit)
  }

  describe("Zipped ordered set lazy operations:") {

    it should behave like sequenceCallsFunctionToComputeLazyValueOnlyOnce(testSuite, Vector(true, false))
  }
}