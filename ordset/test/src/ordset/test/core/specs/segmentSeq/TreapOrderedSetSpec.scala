package ordset.test.core.specs.segmentSeq

import ordset.core.set.OrderedSet
import ordset.core.domain.Domain
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import ordset.test.core.behaviors.segmentSeq.{InspectionBehaviors, NavigationBehaviors, TransformationBehaviors}
import ordset.test.core.samples.segmentSeq.treapOrderedSet._

@RunWith(classOf[JUnitRunner])
class TreapOrderedSetSpec extends AnyFunSpec
  with InspectionBehaviors[Int, Domain.ContinuousUnbounded[Int], Boolean]
  with NavigationBehaviors[Int, Domain.ContinuousUnbounded[Int], Boolean]
  with TransformationBehaviors[Int, Domain.ContinuousUnbounded[Int], Boolean] {

  import ordset.instances.boolean._
  import ordset.instances.int._
  import ordset.test.core.TestRngUtil.Implicits._

  type Dom = Domain.ContinuousUnbounded[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val testSuite = List(
    (1 to 3).flatMap { seed =>
      List(
        new EmptySetSample1[Dom](seed),
        new UniversalSetSample1[Dom](seed),
        new SingleBoundedSetSample1[Dom](seed),
      )
    },
    (1 to 10).flatMap { seed =>
      List(
        new MultiBoundedSetSample1[Dom](seed),
        new MultiBoundedSetSample3[Dom](seed),
        new DegenerateSetSample1[Dom](seed)
      )
    }
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
  }
}