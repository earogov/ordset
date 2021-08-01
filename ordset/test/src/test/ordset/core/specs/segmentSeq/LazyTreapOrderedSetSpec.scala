package test.ordset.core.specs.segmentSeq

import ordset.core.domain.Domain
import ordset.core.set.OrderedSet
import org.junit.runner.RunWith
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.junit.JUnitRunner
import test.ordset.core.behaviors.lazyTreapSeq.LazyTreapSeqBehaviours
import test.ordset.core.domain.BoundSelector
import test.ordset.core.samples.segmentSeq.LazyTreapSeqUtil
import test.ordset.core.samples.segmentSeq.lazyTreapOrderedSet._
import test.ordset.core.samples.segmentSeq.treapOrderedSet.MultiBoundedSetSample3
import test.ordset.core.{RandomUtil, SegmentSeqAssertions, TestRngUtil}

@RunWith(classOf[JUnitRunner])
class LazyTreapOrderedSetSpec extends AnyFunSpec
  with LazyTreapSeqBehaviours[Int, Domain[Int], Boolean] {

  import ordset.core.instances.boolean._
  import ordset.core.instances.int._
  import test.ordset.core.TestRngUtil.Implicits._

  type Dom = Domain[Int]
  type SegmentSeq = OrderedSet[Int, Dom]

  private val lazySuit = List(
    new EmptySetSampleLT1[Dom],
    new SingleBounedSetSampleLT1[Dom],
    new MultiBoundedSetSampleLT1[Dom],
    new MultiBoundedSetSampleLT2[Dom],
  )

//  describe("Lazy treap ordered set specific operations:") {
//
//    it should behave like sequenceProperlyCacheLazyValues(lazySuit)
//
//    it should behave like sequenceHasValidStateAfterSequentialRandomAccess(lazySuit)
//
//    it should behave like sequenceHasValidStateAfterConcurrentRandomAccess(lazySuit)
//  }

  it("random test") {

//    val rngManager = TestRngUtil.defaultRngManager(100)
//    (1 to 10).foreach { _ =>
//      val terms = RandomUtil.randomTerms(10, RandomUtil.Distribution.uniform)(rngManager)
//      println(terms)
//    }
//    println("------------")
//    (1 to 10).foreach { _ =>
//      val terms = RandomUtil.randomTerms(10, RandomUtil.Distribution.smallPreferablePow1)(rngManager)
//      println(terms)
//    }
//    println("------------")
//    (1 to 10).foreach { _ =>
//      val terms = RandomUtil.randomTerms(10, RandomUtil.Distribution.smallPreferablePow2)(rngManager)
//      println(terms)
//    }

    var i = 0
    val boundSelector = implicitly[BoundSelector[Int]]
    val rngManagers = (1L to 1000L).map(seed => TestRngUtil.defaultRngManager(seed)).prepended(TestRngUtil.Implicits.defaultRngManager)
    rngManagers.foreach { rngManager =>
      val treapSample = new MultiBoundedSetSample3[Dom](100)
      val lazySeq = LazyTreapSeqUtil.makeRandomLazySeq(treapSample.sequence)(boundSelector, rngManager)
      LazyTreapSeqUtil.shuffleLazySeq(lazySeq, treapSample.extendedBounds)(rngManager)
//      println(s"Case $i")
//      println(lazySeq.getZippedSeq.toString)
      SegmentSeqAssertions.assertSameSegmentSeq(treapSample.sequence, lazySeq)
      i = i + 1
    }
  }
}