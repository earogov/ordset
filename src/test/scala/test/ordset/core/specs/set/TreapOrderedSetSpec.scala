package test.ordset.core.specs.set

import ordset.core.OrderedSet
import ordset.core.domain.Domain
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.behaviors.SegmentSeqBehaviors
import test.ordset.core.samples.treapOrderedSet._

class TreapOrderedSetSpec extends AnyFunSpec
  with SegmentSeqBehaviors[Int, Domain[Int], Boolean] {

  import ordset.core.instances.Boolean._
  import ordset.core.instances.Int._

  type Dom = Domain[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val testSuite = List(1, 10, 100, 1000, 10000).flatMap { seed =>
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
}