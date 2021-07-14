package test.ordset.core.specs.segmentSeq

import ordset.core.set.OrderedSet
import ordset.core.domain.Domain
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.behaviors.segmentSeq.{InspectionBehaviors, NavigationBehaviors, TransformationBehaviors}
import test.ordset.core.samples.segmentSeq.treapOrderedSet._

@RunWith(classOf[JUnitRunner])
class TreapOrderedSetSpec extends AnyFunSpec
  with InspectionBehaviors[Int, Domain[Int], Boolean]
  with NavigationBehaviors[Int, Domain[Int], Boolean]
  with TransformationBehaviors[Int, Domain[Int], Boolean] {

  import ordset.core.instances.boolean._
  import ordset.core.instances.int._
  import test.ordset.core.TestRngUtil.Implicits._

  type Dom = Domain[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val testSuite = Range(1, 11).flatMap { seed =>
    List(
      new EmptySetSample1[Dom](seed),
      new UniversalSetSample1[Dom](seed),
      new SingleBoundedSetSample1[Dom](seed),
      new MultiBoundedSetSample1[Dom](seed),
      new MultiBoundedSetSample3[Dom](seed),
      new DegenerateSetSample1[Dom](seed)
    )
  }

  describe("Treap based ordered set inspection operations:") {

    it should behave like segmentsHaveNavigationIndicators(testSuite)

    it should behave like segmentsSupportContains(testSuite)

    it should behave like segmentsCanRestrictBound(testSuite)
  }
  
  describe("Treap based ordered set navigation operations:") {

    it should behave like segmentsSupportMovePrevAndNext(testSuite)

    it should behave like segmentsSupportMoveToBound(testSuite)

    it should behave like segmentsSupportMoveToFirstAndLast(testSuite)
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