package ordset.test.specs

import org.scalatest.funspec.AnyFunSpec

import ordset.Bounded

import org.scalatest.Assertions._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

@RunWith(classOf[JUnitRunner])
class BoundedSpec extends AnyFunSpec {

  import BoundedSpec._

  it("should specify set bounded from below") {

    val bounded1 = new Bounded.Below[Int] { 
      override val lowerBound: Int = 0
      override val lowerBoundIncluded: Boolean = true
    }

    validateBoundedBelow(bounded1, 0, true)

    val bounded2 = new Bounded.Below[Int] { 
      override val lowerBound: Int = 0
      override val lowerBoundIncluded: Boolean = false
    }

    validateBoundedBelow(bounded2, 0, false)

    val bounded3 = new Bounded.Below.Including[Int] { 
      override val lowerBound: Int = 0
    }

    validateBoundedBelow(bounded3, 0, true)
  }

  it("should specify set bounded from above") {

      val bounded1 = new Bounded.Above[Int] { 
        override val upperBound: Int = 0
        override val upperBoundIncluded: Boolean = true
      }

      validateBoundedAbove(bounded1, 0, true)

      val bounded2 = new Bounded.Above[Int] { 
        override val upperBound: Int = 0
        override val upperBoundIncluded: Boolean = false
      }

      validateBoundedAbove(bounded2, 0, false)

      val bounded3 = new Bounded.Above.Including[Int] { 
        override val upperBound: Int = 0
      }

      validateBoundedAbove(bounded3, 0, true)
  }

  it("should specify set bounded from below and above") {

    val bounded1 = new Bounded[Int, Int] {
      override val lowerBound: Int = 0
      override val lowerBoundIncluded: Boolean = false
      override val upperBound: Int = 100
      override val upperBoundIncluded: Boolean = true
    }

    validateBounded(bounded1, 0, false, 100, true)

    val bounded2 = new Bounded.Including[Int, Int] {
      override val lowerBound: Int = 0
      override val upperBound: Int = 100
    }

    validateBounded(bounded2, 0, true, 100, true)
  }
}

object BoundedSpec {

  def validateBoundedBelow[E](
    bounded: Bounded.Below[E],
    lowerBound: E,
    lowerIncluded: Boolean
  ): Unit = {
    validateBoundedBelowWithoutReversed(bounded, lowerBound, lowerIncluded)
    validateBoundedAboveWithoutReversed(bounded.reversed, lowerBound, lowerIncluded)
  }

  def validateBoundedAbove[E](
    bounded: Bounded.Above[E],
    upperBound: E,
    upperIncluded: Boolean
  ): Unit = {
    validateBoundedAboveWithoutReversed(bounded, upperBound, upperIncluded)
    validateBoundedBelowWithoutReversed(bounded.reversed, upperBound, upperIncluded)
  }

  def validateBounded[E](
    bounded: Bounded[E, E],
    lowerBound: E,
    lowerIncluded: Boolean,
    upperBound: E,
    upperIncluded: Boolean
  ): Unit = {
    validateBoundedWithoutReversed(bounded, lowerBound, lowerIncluded, upperBound, upperIncluded)
    validateBoundedWithoutReversed(bounded.reversed, upperBound, upperIncluded, lowerBound, lowerIncluded)
  }

  private def validateBoundedBelowWithoutReversed[E](
    bounded: Bounded.Below[E],
    lowerBound: E,
    lowerIncluded: Boolean
  ): Unit = {
    assert(bounded.lowerBound == lowerBound)
    assert(bounded.lowerBoundIncluded == lowerIncluded)
    assert(bounded.lowerBoundExcluded == !lowerIncluded)
  }

  private def validateBoundedAboveWithoutReversed[E](
    bounded: Bounded.Above[E],
    upperBound: E,
    upperIncluded: Boolean
  ): Unit = {
    assert(bounded.upperBound == upperBound)
    assert(bounded.upperBoundIncluded == upperIncluded)
    assert(bounded.upperBoundExcluded == !upperIncluded)
  }

  private def validateBoundedWithoutReversed[E](
    bounded: Bounded[E, E],
    lowerBound: E,
    lowerIncluded: Boolean,
    upperBound: E,
    upperIncluded: Boolean
  ): Unit = {
    validateBoundedBelow(bounded, lowerBound, lowerIncluded)
    validateBoundedAbove(bounded, upperBound, upperIncluded)
  }
}
