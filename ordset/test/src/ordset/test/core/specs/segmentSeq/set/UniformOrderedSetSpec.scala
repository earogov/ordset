package ordset.test.core.specs.segmentSeq.set

import ordset.core.segmentSeq.set.OrderedSet
import ordset.core.domain.Domain
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import ordset.test.core.behaviors.segmentSeq.{InspectionBehaviors, NavigationBehaviors, TransformationBehaviors}
import ordset.test.core.behaviors.segmentSeq.LazyEvalProperties
import ordset.test.core.samples.segmentSeq.set.uniformOrderedSet._

@RunWith(classOf[JUnitRunner])
class UniformOrderedSetSpec extends AnyFunSpec
  with InspectionBehaviors[Int, Domain.ContinuousUnbounded, Boolean]
  with NavigationBehaviors[Int, Domain.ContinuousUnbounded, Boolean]
  with TransformationBehaviors[Int, Domain.ContinuousUnbounded, Boolean]
  with LazyEvalProperties[Int, Domain.ContinuousUnbounded, Boolean] {

  import ordset.givens.boolean._
  import ordset.givens.int._
  import ordset.test.core.TestRngUtil.Givens._

  type Dom[X] = Domain.ContinuousUnbounded[X]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val testSuite = List(
    new EmptySetSample1[Dom](),
    new UniversalSetSample1[Dom](),
  )

  describe("Uniform ordered set inspection operations:") {

    it should behave like segmentsHaveNavigationIndicators(testSuite)

    it should behave like segmentsSupportContains(testSuite)

    it should behave like segmentsCanRestrictBound(testSuite)

    it should behave like supportReturnValueForBound(testSuite)
  }
  
  describe("Uniform ordered set navigation operations:") {

    it should behave like segmentsSupportMovePrevAndNext(testSuite)

    it should behave like supportMoveToBound(testSuite)

    it should behave like supportMoveToFirstAndLast(testSuite)
  }

  describe("Uniform ordered set transformation operations:") {

    it should behave like segmentSeqCanBePrepended(testSuite)

    it should behave like segmentSeqCanBePrependedBelowBound(testSuite)

    it should behave like segmentSeqCanBeAppended(testSuite)

    it should behave like segmentSeqCanBeAppendedAboveBound(testSuite)

    it should behave like segmentSeqCanBeSliced(testSuite)

    it should behave like segmentCanBePatched(testSuite)

    it should behave like segmentSeqCanBeConvertedIntoStrictSequence(testSuite)
  }

  describe("Uniform ordered set lazy operations:") {

    it should behave like sequenceCallsFunctionToComputeLazyValueOnlyOnce(testSuite, Vector(true, false))
  }
}