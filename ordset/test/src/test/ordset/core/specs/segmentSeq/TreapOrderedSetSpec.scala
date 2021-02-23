package test.ordset.core.specs.segmentSeq

import ordset.core.OrderedSet
import ordset.core.domain.Domain
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.behaviors.segmentSeq.{NavigationBehaviors, TransformationBehaviors}
import test.ordset.core.samples.segmentSeq.treapOrderedSet._

class TreapOrderedSetSpec extends AnyFunSpec
  with NavigationBehaviors[Int, Domain[Int], Boolean]
  with TransformationBehaviors[Int, Domain[Int], Boolean] {

  import ordset.core.instances.boolean._
  import ordset.core.instances.int._

  type Dom = Domain[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val testSuite = Range(1, 20).flatMap { seed =>
    List(
      new EmptySetSample1[Dom](seed),
      new UniversalSetSample1[Dom](seed),
      new SingleBoundedSetSample1[Dom](seed),
      new MultiBoundedSetSample3[Dom](seed),
      new DegenerateSetSample1[Dom](seed)
    )
  }

  describe("Treap based ordered set navigation operations") {

    it should behave like segmentsSupportMovePrevAndNext(testSuite)

    it should behave like segmentsSupportMoveToBound(testSuite)

    it should behave like segmentsSupportMoveToFirstAndLast(testSuite)

    it should behave like segmentsHaveNextAndPrevIndicators(testSuite)
  }

  describe("Treap based ordered set transformation operations") {

    it should behave like segmentSeqCanBeSliced(testSuite)
  }
}