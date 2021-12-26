package ordset.test.core.specs.range

import ordset.Eq
import ordset.core.range.{RangeAlgebra, SimpleRange, SimpleRangeFactory, RangeFactory}
import ordset.test.core.RangeAssertions._

import org.scalatest.Assertions._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

@RunWith(classOf[JUnitRunner])
class RangeAlgebraSpec extends AnyFunSpec {

  import ordset.implementations.int._

  private val alg: RangeAlgebra[Int] = implicitly
  private val eq: Eq[SimpleRange[Int]] = implicitly
  private val range: SimpleRangeFactory[Int] = implicitly

  it("should support `isValid` check") {
    // Non-empty range
    assert(alg.isValid(SimpleRange(1, 3)))
    assert(alg.isValid(SimpleRange(1, 1)))
    assert(!alg.isValid(SimpleRange(1, -1)))
    assert(alg.isValidNE(SimpleRange(1, 3)))
    assert(alg.isValidNE(SimpleRange(1, 1)))
    assert(!alg.isValidNE(SimpleRange(1, -1)))

    // Empty range
    assert(alg.isValid(SimpleRange.Empty))
  }

  it("should support `contains` check") {
    // Non-empty range
    assert(alg.contains(SimpleRange(1, 3), 1))
    assert(alg.contains(SimpleRange(1, 3), 2))
    assert(alg.contains(SimpleRange(1, 3), 3))
    assert(!alg.contains(SimpleRange(1, 3), 0))
    assert(!alg.contains(SimpleRange(1, 3), 4))
    assert(alg.containsNE(SimpleRange(1, 3), 1))
    assert(alg.containsNE(SimpleRange(1, 3), 2))
    assert(alg.containsNE(SimpleRange(1, 3), 3))
    assert(!alg.containsNE(SimpleRange(1, 3), 0))
    assert(!alg.containsNE(SimpleRange(1, 3), 4))

    // Empty range
    assert(!alg.contains(SimpleRange.empty, 0))
    assert(!alg.contains(SimpleRange.empty, 10))
  }

  it("should support `restrict` operation") {
    assert(alg.restrictNE(SimpleRange(1, 3), 1) == 1)
    assert(alg.restrictNE(SimpleRange(1, 3), 2) == 2)
    assert(alg.restrictNE(SimpleRange(1, 3), 3) == 3)
    assert(alg.restrictNE(SimpleRange(1, 3), 0) == 1)
    assert(alg.restrictNE(SimpleRange(1, 3), 4) == 3)
  }

  it("should support `takeAbove` operation") {
    assertSameRange(SimpleRange(1, 3), alg.takeAbove(SimpleRange(1, 3), 0)(range))
    assertSameRange(SimpleRange(1, 3), alg.takeAbove(SimpleRange(1, 3), 1)(range))
    assertSameRange(SimpleRange(2, 3), alg.takeAbove(SimpleRange(1, 3), 2)(range))
    assertSameRange(SimpleRange(3, 3), alg.takeAbove(SimpleRange(1, 3), 3)(range))
    assertSameRange(SimpleRange.empty, alg.takeAbove(SimpleRange(1, 3), 4)(range))
    assertSameRange(SimpleRange(1, 3), alg.takeAboveNE(SimpleRange(1, 3), 0)(range))
    assertSameRange(SimpleRange(1, 3), alg.takeAboveNE(SimpleRange(1, 3), 1)(range))
    assertSameRange(SimpleRange(2, 3), alg.takeAboveNE(SimpleRange(1, 3), 2)(range))
    assertSameRange(SimpleRange(3, 3), alg.takeAboveNE(SimpleRange(1, 3), 3)(range))
    assertSameRange(SimpleRange.empty, alg.takeAboveNE(SimpleRange(1, 3), 4)(range))
  }

  it("should support `takeBelow` operation") {
    assertSameRange(SimpleRange(1, 3), alg.takeBelow(SimpleRange(1, 3), 4)(range))
    assertSameRange(SimpleRange(1, 3), alg.takeBelow(SimpleRange(1, 3), 3)(range))
    assertSameRange(SimpleRange(1, 2), alg.takeBelow(SimpleRange(1, 3), 2)(range))
    assertSameRange(SimpleRange(1, 1), alg.takeBelow(SimpleRange(1, 3), 1)(range))
    assertSameRange(SimpleRange.empty, alg.takeBelow(SimpleRange(1, 3), 0)(range))
    assertSameRange(SimpleRange(1, 3), alg.takeBelowNE(SimpleRange(1, 3), 4)(range))
    assertSameRange(SimpleRange(1, 3), alg.takeBelowNE(SimpleRange(1, 3), 3)(range))
    assertSameRange(SimpleRange(1, 2), alg.takeBelowNE(SimpleRange(1, 3), 2)(range))
    assertSameRange(SimpleRange(1, 1), alg.takeBelowNE(SimpleRange(1, 3), 1)(range))
    assertSameRange(SimpleRange.empty, alg.takeBelowNE(SimpleRange(1, 3), 0)(range))
  }

  it("should support `cross` operation") {
    validateCross(SimpleRange(1, 3), SimpleRange(1, 3), SimpleRange(1, 3))
    validateCross(SimpleRange(1, 3), SimpleRange(1, 3), SimpleRange(0, 4))
    validateCross(SimpleRange(1, 3), SimpleRange(1, 5), SimpleRange(0, 3))
    validateCross(SimpleRange(1, 1), SimpleRange(1, 3), SimpleRange(1, 1))
    validateCross(SimpleRange(3, 3), SimpleRange(1, 3), SimpleRange(3, 3))
    validateCross(SimpleRange(2, 2), SimpleRange(1, 2), SimpleRange(2, 3))
    validateCross(SimpleRange.empty, SimpleRange(-1, 0), SimpleRange(4, 5))
  }

  it("should support `span` operation") {
    validateSpan(SimpleRange(1, 3), SimpleRange(1, 3), SimpleRange(1, 3))
    validateSpan(SimpleRange(1, 3), SimpleRange(1, 3), SimpleRange(2, 2))
    validateSpan(SimpleRange(1, 3), SimpleRange(1, 2), SimpleRange(2, 3))
    validateSpan(SimpleRange(1, 3), SimpleRange(1, 1), SimpleRange(3, 3))
    validateSpan(SimpleRange(1, 3), SimpleRange(1, 3), SimpleRange.empty)
    validateSpan(SimpleRange.empty, SimpleRange.empty, SimpleRange.empty)
  }

  private def validateCross(expected: SimpleRange[Int], x: SimpleRange[Int], y: SimpleRange[Int]): Unit = {
    assertSameRange(expected, alg.cross(x, y)(range))
    assertSameRange(expected, alg.cross(y, x)(range))
    (x, y) match {
      case (x: SimpleRange.NonEmpty[Int], y: SimpleRange.NonEmpty[Int]) =>
        assertSameRange(expected, alg.crossNE(x, y)(range))
        assertSameRange(expected, alg.crossNE(y, x)(range))
      case _ =>
        // nothing to do
    }
  }
  private def validateSpan(expected: SimpleRange[Int], x: SimpleRange[Int], y: SimpleRange[Int]): Unit = {
    assertSameRange(expected, alg.span(x, y)(range))
    assertSameRange(expected, alg.span(y, x)(range))
    (x, y) match {
      case (x: SimpleRange.NonEmpty[Int], y: SimpleRange.NonEmpty[Int]) =>
        assertSameRange(expected, alg.spanNE(x, y)(range))
        assertSameRange(expected, alg.spanNE(y, x)(range))
      case _ =>
        // nothing to do
    }
  }
}
