package ordset.test.core.specs.segmentSeq.set

import ordset.core.segmentSeq.set.OrderedSet
import ordset.core.domain.Domain
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import ordset.test.core.behaviors.segmentSeq.{InspectionBehaviors, NavigationBehaviors, TransformationBehaviors}
import ordset.test.core.behaviors.segmentSeq.LazyEvalProperties
import ordset.test.core.samples.segmentSeq.set.treapOrderedSet._

@RunWith(classOf[JUnitRunner])
class TreapOrderedSetSpec extends AnyFunSpec
  with InspectionBehaviors[Int, Domain.ContinuousUnbounded, Boolean]
  with NavigationBehaviors[Int, Domain.ContinuousUnbounded, Boolean]
  with TransformationBehaviors[Int, Domain.ContinuousUnbounded, Boolean]
  with LazyEvalProperties[Int, Domain.ContinuousUnbounded, Boolean] {

  import ordset.givens.boolean._
  import ordset.givens.int._
  import ordset.test.core.TestRngUtil.Givens._

  type Dom[X] = Domain.ContinuousUnbounded[X]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val uniformSuite = List(
    new EmptySetSample1[Dom](100),
    new UniversalSetSample1[Dom](100)
  )

  private val testSuite = List(
    uniformSuite,
    (1 to 2).flatMap { seed =>
      List(
        new SingleBoundedSetSample1[Dom](seed)
      )
    },
    (1 to 10).flatMap { seed =>
      List(
        new MultiBoundedSetSample1[Dom](seed),
        new MultiBoundedSetSample3[Dom](seed),
        new DegenerateSetSample1[Dom](seed)
      )
    },
    // Inverted sets
    (20 to 20).flatMap { seed =>
      List(
        new MultiBoundedSetSample3_inv[Dom](seed),
      )
    }
  ).flatten

  private val lazyPropertiesSuite = List(
    uniformSuite,
    List(
      new SingleBoundedSetSample1[Dom](100),
      new MultiBoundedSetSample1[Dom](100),
      new MultiBoundedSetSample3[Dom](100),
      new DegenerateSetSample1[Dom](100)
    )
  ).flatten

  describe("Treap based ordered set inspection operations:") {

    it should behave like segmentsHaveNavigationIndicators(testSuite)

    it should behave like segmentsSupportContains(testSuite)

    it should behave like segmentsCanRestrictBound(testSuite)

    it should behave like supportReturnValueForBound(testSuite)
  }
  
  describe("Treap based ordered set navigation operations:") {

    it should behave like segmentsSupportMovePrevAndNext(testSuite)

    it should behave like supportMoveToBound(testSuite)

    it should behave like supportMoveToFirstAndLast(testSuite)
  }

  describe("Treap based ordered set transformation operations:") {

    it should behave like segmentSeqCanBePrepended(testSuite)

    it should behave like segmentSeqCanBePrependedBelowBound(testSuite)

    it should behave like segmentSeqCanBeAppended(testSuite)

    it should behave like segmentSeqCanBeAppendedAboveBound(testSuite)

    it should behave like segmentSeqCanBeSliced(testSuite)

    it should behave like segmentCanBePatched(testSuite)

    it should behave like segmentSeqCanBeConvertedIntoStrictSequence(testSuite)
  }

  describe("Treap based ordered set lazy operations:") {

    it should behave like sequenceCallsFunctionToComputeLazyValueOnlyOnce(lazyPropertiesSuite, Vector(true, false))
  }
}