package test.ordset.core.specs.segmentSeq

import ordset.core.OrderedSet
import ordset.core.domain.Domain
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.behaviors.segmentSeq.{NavigationBehaviors, TransformationBehaviors}
import test.ordset.core.samples.segmentSeq.arrayOrderedSet._

class ArrayOrderedSetSpec extends AnyFunSpec
  with NavigationBehaviors[Int, Domain[Int], Boolean]
  with TransformationBehaviors[Int, Domain[Int], Boolean] {

  import ordset.core.instances.boolean._
  import ordset.core.instances.int._
  import test.ordset.core.TestRngUtil.Implicits._

  type Dom = Domain[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val testSuite = List(
    new EmptySetSample1[Dom],
    new UniversalSetSample1[Dom],
    new SingleBoundedSetSample1[Dom],
    new MultiBoundedSetSample1[Dom],
    new DegenerateSetSample1[Dom]
  )
  
  private val patchedSuite = List(
    new MultiBoundedSetSample1[Dom]
  )

  describe("Array based ordered set navigation operations") {

    it should behave like segmentsSupportMovePrevAndNext(testSuite)

    it should behave like segmentsSupportMoveToBound(testSuite)

    it should behave like segmentsSupportMoveToFirstAndLast(testSuite)

    it should behave like segmentsHaveNextAndPrevIndicators(testSuite)
  }

  describe("Array based ordered set transformation operations") {

    it should behave like segmentSeqCanBeAppended(testSuite)

    it should behave like segmentSeqCanBeSliced(testSuite)
    
    it should behave like segmentCanBePatched(patchedSuite)
  }
}