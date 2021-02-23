package test.ordset.core.specs.segmentSeq

import ordset.core.OrderedSet
import ordset.core.domain.Domain
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.behaviors.segmentSeq.{NavigationBehaviors, TransformationBehaviors}
import test.ordset.core.samples.segmentSeq.uniformOrderedSet._

class UniformOrderedSetSpec extends AnyFunSpec
  with NavigationBehaviors[Int, Domain[Int], Boolean]
  with TransformationBehaviors[Int, Domain[Int], Boolean] {

  import ordset.core.instances.Boolean._
  import ordset.core.instances.Int._

  type Dom = Domain[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val testSuite = List(
    new EmptySetSample1[Dom](),
    new UniversalSetSample1[Dom](),
  )

  describe("Uniform ordered set navigation operations") {

    it should behave like segmentsSupportMovePrevAndNext(testSuite)

    it should behave like segmentsSupportMoveToBound(testSuite)

    it should behave like segmentsSupportMoveToFirstAndLast(testSuite)

    it should behave like segmentsHaveNextAndPrevIndicators(testSuite)
  }

  describe("Uniform ordered set transformation operations") {

    it should behave like segmentSeqCanBeAppended(testSuite)
  }
}