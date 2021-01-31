package test.ordset.core.specs.segmentSeq

import ordset.core.OrderedSet
import ordset.core.domain.Domain
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.behaviors.segmentSeq.{NavigationBehaviors, TransformationBehaviors}
import test.ordset.core.samples.segmentSeq.arrayOrderedSet._

class ArrayOrderedSetSpec extends AnyFunSpec
  with NavigationBehaviors[Int, Domain[Int], Boolean]
  with TransformationBehaviors[Int, Domain[Int], Boolean] {

  import ordset.core.instances.Boolean._
  import ordset.core.instances.Int._

  type Dom = Domain[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val navTestSuite = List(
    new EmptySetSample1[Dom](),
    new UniversalSetSample1[Dom](),
    new SingleBoundedSetSample1[Dom],
    new MultiBoundedSetSample1[Dom],
    new DegenerateSetSample1[Dom]
  )

  private val transTestSuite = List(
    new MultiBoundedSetSample1[Dom]
  )

  describe("Array based ordered set navigation operations") {

    it should behave like segmentsSupportMovePrevAndNext(navTestSuite)

    it should behave like segmentsSupportMoveToBound(navTestSuite)

    it should behave like segmentsSupportMoveToFirstAndLast(navTestSuite)

    it should behave like segmentsHaveNextAndPrevIndicators(navTestSuite)
  }

  describe("Array based ordered set transformation operations") {

    it should behave like segmentSeqCanBeAppended(transTestSuite)
  }
}