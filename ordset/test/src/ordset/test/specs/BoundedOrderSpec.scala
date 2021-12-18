package ordset.test.specs

import ordset.{Eq, Order, BoundedOrder}
import org.scalatest.funspec.AnyFunSpec

import org.scalatest.Assertions._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import cats.instances.set

@RunWith(classOf[JUnitRunner])
class OrderSpec extends AnyFunSpec {

  import BoundedOrderSpec._
  import ordset.givens.int._

  it("should specify ordered set bounded from below") {

    val bounded1 = new BoundedOrder.Below[Int, Int] { 
      override val lowerBound: Int = 0
      override val lowerBoundIncluded: Boolean = true
      override def compare(x: Int, y: Int): Int = intNaturalOrder.compare(x, y)
    }

    validateBoundedBelowOrder(bounded1, 0, true, List(-3, -2, -1), List(0, 1, 2, 3))

    val bounded2 = new BoundedOrder.Below[Int, Int] { 
      override val lowerBound: Int = 0
      override val lowerBoundIncluded: Boolean = false
      override def compare(x: Int, y: Int): Int = intNaturalOrder.compare(x, y)
    }

    validateBoundedBelowOrder(bounded2, 0, false, List(-3, -2, -1, 0), List(1, 2, 3))

    val bounded3 = new BoundedOrder.Below.Including[Int, Int] { 
      override val lowerBound: Int = 0
      override def compare(x: Int, y: Int): Int = intNaturalOrder.compare(x, y)
    }

    validateBoundedBelowOrder(bounded3, 0, true, List(-3, -2, -1), List(0, 1, 2, 3))
  }

  it("should specify ordered set bounded from above") {

      val bounded1 = new BoundedOrder.Above[Int, Int] { 
        override val upperBound: Int = 0
        override val upperBoundIncluded: Boolean = true
        override def compare(x: Int, y: Int): Int = intNaturalOrder.compare(x, y)
      }

      validateBoundedAboveOrder(bounded1, 0, true, List(-3, -2, -1, 0), List(1, 2, 3))

      val bounded2 = new BoundedOrder.Above[Int, Int] { 
        override val upperBound: Int = 0
        override val upperBoundIncluded: Boolean = false
        override def compare(x: Int, y: Int): Int = intNaturalOrder.compare(x, y)
      }

      validateBoundedAboveOrder(bounded2, 0, false, List(-3, -2, -1), List(0, 1, 2, 3))

      val bounded3 = new BoundedOrder.Above.Including[Int, Int] { 
        override val upperBound: Int = 0
        override def compare(x: Int, y: Int): Int = intNaturalOrder.compare(x, y)
      }

      validateBoundedAboveOrder(bounded3, 0, true, List(-3, -2, -1, 0), List(1, 2, 3))
  }

  it("should specify ordered set bounded from below and above") {

    val bounded1 = new BoundedOrder[Int, Int, Int] {
      override val lowerBound: Int = 0
      override val lowerBoundIncluded: Boolean = false
      override val upperBound: Int = 100
      override val upperBoundIncluded: Boolean = true
      override def compare(x: Int, y: Int): Int = intNaturalOrder.compare(x, y)
    }

    validateBoundedOrder(bounded1, 0, false, 100, true, List(-3, -2, -1, 0), List(1, 2, 3, 100), List(101, 102, 103))

    val bounded2 = new BoundedOrder.Including[Int, Int, Int] {
      override val lowerBound: Int = 0
      override val upperBound: Int = 100
      override def compare(x: Int, y: Int): Int = intNaturalOrder.compare(x, y)
    }

    validateBoundedOrder(bounded2, 0, true, 100, true, List(-3, -2, -1), List(0, 1, 2, 3, 100), List(101, 102, 103))
  }
}

object BoundedOrderSpec {

  def validateBoundedBelowOrder[E](
    boundedOrd: BoundedOrder.Below[E, E],
    lowerBound: E,
    lowerIncluded: Boolean,
    belowElements: List[E],
    setElements: List[E]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateBoundedBelowOrderWithoutReversed(boundedOrd, lowerBound, lowerIncluded, belowElements, setElements)
    validateBoundedAboveOrderWithoutReversed(boundedOrd.reversed, lowerBound, lowerIncluded, setElements, belowElements)
  }

  def validateBoundedAboveOrder[E](
    boundedOrd: BoundedOrder.Above[E, E],
    upperBound: E,
    upperIncluded: Boolean,
    setElements: List[E],
    aboveElements: List[E]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateBoundedAboveOrderWithoutReversed(boundedOrd, upperBound, upperIncluded, setElements, aboveElements)
    validateBoundedBelowOrderWithoutReversed(boundedOrd.reversed, upperBound, upperIncluded, aboveElements, setElements)
  }

  def validateBoundedOrder[E](
    boundedOrd: BoundedOrder[E, E, E],
    lowerBound: E,
    lowerIncluded: Boolean,
    upperBound: E,
    upperIncluded: Boolean,
    belowElements: List[E],
    setElements: List[E],
    aboveElements: List[E]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateBoundedOrderWithoutReversed(
      boundedOrd, lowerBound, lowerIncluded, upperBound, upperIncluded, belowElements, setElements, aboveElements
    )
    validateBoundedOrderWithoutReversed(
      boundedOrd.reversed, upperBound, upperIncluded, lowerBound, lowerIncluded, aboveElements, setElements, belowElements
    )
  }

  private def validateBoundedBelowOrderWithoutReversed[E](
    boundedOrd: BoundedOrder.Below[E, E],
    lowerBound: E,
    lowerIncluded: Boolean,
    belowElements: List[E],
    setElements: List[E]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    BoundedSpec.validateBoundedBelow(boundedOrd, lowerBound, lowerIncluded)

    validateElementsAboveBound(boundedOrd, lowerBound, lowerIncluded, setElements)
    validateElementsBelowBound(boundedOrd, lowerBound, !lowerIncluded, belowElements)

    if lowerIncluded then assert(boundedOrd.includes(lowerBound))
    else assert(!boundedOrd.includes(lowerBound))

    setElements.foreach { e =>
      assert(boundedOrd.includes(e))
      assert(boundedOrd.aboveLowerBound(e))
    }

    belowElements.foreach { e =>
      assert(!boundedOrd.includes(e))
      assert(boundedOrd.belowLowerBound(e))
    }

    boundedOrd match {
      case boundedOrd: BoundedOrder.Below.Including[E, E] =>
        assert(lowerIncluded)
        assert(boundedOrd.isLeastElement(lowerBound))

        setElements.foreach { e =>
          assert(eq.eqv(boundedOrd.restrict(e), e))
        }

        belowElements.foreach { e =>
          assert(eq.eqv(boundedOrd.restrict(e), lowerBound))
        }
      case _ => 
        // nothing to do
    }
  }

  private def validateBoundedAboveOrderWithoutReversed[E](
    boundedOrd: BoundedOrder.Above[E, E],
    upperBound: E,
    upperIncluded: Boolean,
    setElements: List[E],
    aboveElements: List[E]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    BoundedSpec.validateBoundedAbove(boundedOrd, upperBound, upperIncluded)

    validateElementsBelowBound(boundedOrd, upperBound, upperIncluded, setElements)
    validateElementsAboveBound(boundedOrd, upperBound, !upperIncluded, aboveElements)

    if upperIncluded then assert(boundedOrd.includes(upperBound))
    else assert(!boundedOrd.includes(upperBound))

    setElements.foreach { e =>
      assert(boundedOrd.includes(e))
      assert(boundedOrd.belowUpperBound(e))
    }

    aboveElements.foreach { e =>
      assert(!boundedOrd.includes(e))
      assert(boundedOrd.aboveUpperBound(e))
    }

    boundedOrd match {
      case boundedOrd: BoundedOrder.Above.Including[E, E] =>
        assert(upperIncluded)
        assert(boundedOrd.isGreatestElement(upperBound))

        setElements.foreach { e =>
          assert(eq.eqv(boundedOrd.restrict(e), e))
        }

        aboveElements.foreach { e =>
          assert(eq.eqv(boundedOrd.restrict(e), upperBound))
        }
      case _ => 
        // nothing to do
    }
  }

  private def validateBoundedOrderWithoutReversed[E](
    boundedOrd: BoundedOrder[E, E, E],
    lowerBound: E,
    lowerIncluded: Boolean,
    upperBound: E,
    upperIncluded: Boolean,
    belowElements: List[E],
    setElements: List[E],
    aboveElements: List[E]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateBoundedBelowOrder(boundedOrd, lowerBound, lowerIncluded, belowElements, setElements)
    validateBoundedAboveOrder(boundedOrd, upperBound, upperIncluded, setElements, aboveElements)
  }

  private def validateElementsAboveBound[E](
    ord: Order[E],
    lowerBound: E,
    lowerIncluded: Boolean,
    setElements: List[E]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    val ordering = ord.toOrdering

    assert(ord.eqv(lowerBound, lowerBound))
    assert(ord.compare(lowerBound, lowerBound) == 0)

    assert(ordering.equiv(lowerBound, lowerBound))
    assert(ordering.compare(lowerBound, lowerBound) == 0)

    setElements.foreach { e =>

      if !lowerIncluded then {
        assert(ord.lt(lowerBound, e))
        assert(ord.lteqv(lowerBound, e))
        assert(ord.compare(lowerBound, e) < 0)

        assert(ord.gt(e, lowerBound))
        assert(ord.gteqv(e, lowerBound))
        assert(ord.compare(e, lowerBound) > 0)

        assert(ordering.lt(lowerBound, e))
        assert(ordering.lteq(lowerBound, e))
        assert(ordering.compare(lowerBound, e) < 0)

        assert(ordering.gt(e, lowerBound))
        assert(ordering.gteq(e, lowerBound))
        assert(ordering.compare(e, lowerBound) > 0)

        assert(ord.neqv(lowerBound, e))
        assert(ord.neqv(e, lowerBound))
      } else {
        assert(ord.lteqv(lowerBound, e))
        assert(ord.compare(lowerBound, e) <= 0)

        assert(ord.gteqv(e, lowerBound))
        assert(ord.compare(e, lowerBound) >= 0)

        assert(ordering.lteq(lowerBound, e))
        assert(ordering.compare(lowerBound, e) <= 0)

        assert(ordering.gteq(e, lowerBound))
        assert(ordering.compare(e, lowerBound) >= 0)
      }

      assert(ord.eqv(e, e))
      assert(ord.compare(e, e) == 0) 

      assert(eq.eqv(ord.min(lowerBound, e), lowerBound))
      assert(eq.eqv(ord.min(e, lowerBound), lowerBound))

      assert(eq.eqv(ord.max(lowerBound, e), e))
      assert(eq.eqv(ord.max(e, lowerBound), e))

      assert(ordering.equiv(e, e))
      assert(ordering.compare(e, e) == 0) 

      assert(eq.eqv(ordering.min(lowerBound, e), lowerBound))
      assert(eq.eqv(ordering.min(e, lowerBound), lowerBound))

      assert(eq.eqv(ordering.max(lowerBound, e), e))
      assert(eq.eqv(ordering.max(e, lowerBound), e))
    }
  }

  private def validateElementsBelowBound[E](
    ord: Order[E],
    upperBound: E,
    upperIncluded: Boolean,
    setElements: List[E]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    val ordering = ord.toOrdering

    assert(ord.eqv(upperBound, upperBound))
    assert(ord.compare(upperBound, upperBound) == 0)

    assert(ordering.equiv(upperBound, upperBound))
    assert(ordering.compare(upperBound, upperBound) == 0)

    setElements.foreach { e =>

      if !upperIncluded then {
        assert(ord.gt(upperBound, e))
        assert(ord.gteqv(upperBound, e))
        assert(ord.compare(upperBound, e) > 0)

        assert(ord.lt(e, upperBound))
        assert(ord.lteqv(e, upperBound))
        assert(ord.compare(e, upperBound) < 0)

        assert(ordering.gt(upperBound, e))
        assert(ordering.gteq(upperBound, e))
        assert(ordering.compare(upperBound, e) > 0)

        assert(ordering.lt(e, upperBound))
        assert(ordering.lteq(e, upperBound))
        assert(ordering.compare(e, upperBound) < 0)

        assert(ord.neqv(upperBound, e))
        assert(ord.neqv(e, upperBound))
      } else {
        assert(ord.gteqv(upperBound, e))
        assert(ord.compare(upperBound, e) >= 0)

        assert(ord.lteqv(e, upperBound))
        assert(ord.compare(e, upperBound) <= 0)

        assert(ordering.gteq(upperBound, e))
        assert(ordering.compare(upperBound, e) >= 0)

        assert(ordering.lteq(e, upperBound))
        assert(ordering.compare(e, upperBound) <= 0)
      }

      assert(ord.eqv(e, e))
      assert(ord.compare(e, e) == 0) 

      assert(eq.eqv(ord.min(upperBound, e), e))
      assert(eq.eqv(ord.min(e, upperBound), e))

      assert(eq.eqv(ord.max(upperBound, e), upperBound))
      assert(eq.eqv(ord.max(e, upperBound), upperBound))

      assert(ordering.equiv(e, e))
      assert(ordering.compare(e, e) == 0) 

      assert(eq.eqv(ordering.min(upperBound, e), e))
      assert(eq.eqv(ordering.min(e, upperBound), e))

      assert(eq.eqv(ordering.max(upperBound, e), upperBound))
      assert(eq.eqv(ordering.max(e, upperBound), upperBound))
    }
  }
}
