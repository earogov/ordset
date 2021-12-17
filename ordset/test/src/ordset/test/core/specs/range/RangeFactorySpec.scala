package ordset.test.core.specs.range

import ordset.Hash
import ordset.core.range._
import ordset.test.core.RangeAssertions._

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

@RunWith(classOf[JUnitRunner])
class RangeFactorySpec extends AnyFunSpec {

  import ordset.implementations._
  import ordset.givens.int._
  import ordset.givens.string._

  it("unbounded domain") {

    implicit val range: SimpleRangeFactory[Int] = SimpleRangeFactory.unbounded

    // Non-empty range
    validateRange(SimpleRange(5, 5), 5, 5)
    validateRange(SimpleRange(0, 10), 0, 10)

    // Empty range
    defaultEmptyRangeTests
  }

  it("bounded below domain") {

    val intOrder = int.tryNaturalOrderWithBounds(0, Int.MaxValue).get
    implicit val range: SimpleRangeFactory[Int] = SimpleRangeFactory.boundedBelow(intOrder)

    // Non-empty range
    validateRange(SimpleRange(5, 5), 5, 5)
    validateRange(SimpleRange(5, 10), 5, 10)
    validateRange(SimpleRange(0, 5), 0, 5)
    validateRange(SimpleRange(0, 5), -5, 5)
    validateRange(SimpleRange(0, 0), -10, 0)

    // Empty range
    defaultEmptyRangeTests
    validateEmptyRange(-10, -5)
    validateEmptyRange(-15, -15)
  }

  it("bounded above domain") {

    val intOrder = int.tryNaturalOrderWithBounds(Int.MinValue, 0).get
    implicit val range: SimpleRangeFactory[Int] = SimpleRangeFactory.boundedAbove(intOrder)
    
    // Non-empty range
    validateRange(SimpleRange(-5, -5), -5, -5)
    validateRange(SimpleRange(-10, -5), -10, -5)
    validateRange(SimpleRange(-5, 0), -5, -0)
    validateRange(SimpleRange(-5, 0), -5, 10)
    validateRange(SimpleRange(0, 0), 0, 10)

    // Empty range
    defaultEmptyRangeTests
    validateEmptyRange(5, 10)
    validateEmptyRange(15, 15)
  }

  it("bounded domain") {

    val intOrd = int.tryNaturalOrderWithBounds(0, 10).get
    implicit val range: SimpleRangeFactory[Int] = SimpleRangeFactory.bounded(intOrd)

    // Non-empty range
    validateRange(SimpleRange(5, 5), 5, 5)
    validateRange(SimpleRange(2, 8), 2, 8)
    validateRange(SimpleRange(0, 10), 0, 10)
    validateRange(SimpleRange(0, 10), -2, 12)
    validateRange(SimpleRange(0, 5), -2, 5)
    validateRange(SimpleRange(5, 10), 5, 12)
    validateRange(SimpleRange(0, 0), -5, 0)
    validateRange(SimpleRange(10, 10), 10, 15)

    // Empty range
    defaultEmptyRangeTests
    validateEmptyRange(-10, -5)
    validateEmptyRange(15, 20)
  }

  private def defaultEmptyRangeTests(
    implicit
    range: RangeFactory[Int, SimpleRange[Int]],
    hash: Hash[SimpleRange[Int]]
  ): Unit = {
    assertSameRange[Int, SimpleRange[Int]](SimpleRange.empty, range.empty)
    assertSameRange(SimpleRange.empty, range.like(SimpleRange.empty))
    validateEmptyRange(10, 0)
  }

  private def validateRange[E](
    expected: SimpleRange[E], 
    lower: E, 
    upper: E
  )(
    implicit 
    range: RangeFactory[E, SimpleRange[E]],
    hash: Hash[SimpleRange[E]]
  ): Unit = {
    assertSameRange(expected, range.between(lower, upper))
    assertSameRange(expected, range.like(SimpleRange(lower, upper)))
    assertSameRange(expected, range.likeNE(SimpleRange(lower, upper)))
  }

  private def validateEmptyRange[E](
    lower: E, 
    upper: E
  )(
    implicit 
    range: RangeFactory[E, SimpleRange[E]],
    hash: Hash[SimpleRange[E]]
  ): Unit =
    validateRange(SimpleRange.Empty, lower, upper)
}
