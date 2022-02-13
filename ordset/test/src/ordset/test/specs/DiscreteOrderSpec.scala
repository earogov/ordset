package ordset.test.specs

import ordset.{Eq, DiscreteOrder}
import ordset.test.AdjacentElements

import org.scalatest.Assertions._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

@RunWith(classOf[JUnitRunner])
class DiscreteOrderSpec extends AnyFunSpec {

  import DiscreteOrderSpec._

  val intOrdering = implicitly[Ordering[Int]]
  val bigIntOrdering = implicitly[Ordering[BigInt]]

  it("should specify discrete ordered set") {

    val discreteOrd1 = new DiscreteOrder[Int] { 
      override def successorOrNull(x: Int): Int | Null = if hasSuccessor(x) then x + 1 else null
      override def hasSuccessor(x: Int) = x < 10
      override def predecessorOrNull(x: Int): Int | Null = if hasPredecessor(x) then x - 1 else null
      override def hasPredecessor(x: Int) = x > 0
      override def compare(x: Int, y: Int): Int = intOrdering.compare(x, y)
    }

    validateDiscreteOrder(discreteOrd1, List((null, -1), (null, 0), (0, 1), (1, 2), (9, 10), (11, null), (12, null)))
  }

  it("should specify discrete infinite unbounded ordered set") {

    val discreteOrd1 = new DiscreteOrder.InfiniteUnbounded[BigInt] { 
      override def successor(x: BigInt): BigInt = x + 1
      override def predecessor(x: BigInt): BigInt = x - 1
      override def compare(x: BigInt, y: BigInt): Int = bigIntOrdering.compare(x, y)
    }

    validateDiscreteOrder(
      discreteOrd1, 
      List(
        (BigInt(-2), BigInt(-1)), 
        (BigInt(-1), BigInt(0)), 
        (BigInt(0), BigInt(1)), 
        (BigInt(1), BigInt(2)), 
        (BigInt(9), BigInt(10)), 
        (BigInt(11), BigInt(12)), 
        (BigInt(12), BigInt(13))
      )
    )
  }

  it("should specify discrete finite from below ordered set") {

    val discreteOrd1 = new DiscreteOrder.Finite.Below[Int, Int] { 
      override def successorOrNull(x: Int): Int | Null = x + 1
      override def hasSuccessor(x: Int) = true
      override def predecessorOrNull(x: Int): Int | Null = if hasPredecessor(x) then x - 1 else null
      override def lowerBound: Int = 0
      override def compare(x: Int, y: Int): Int = intOrdering.compare(x, y)
    }

    validateDiscreteFiniteBelowOrder(
      discreteOrd1,
      0,
      List((null, -1), (null, 0), (0, 1), (1, 2), (9, 10), (11, 12)),
      List(-3, -2, -1),
      List(0, 1, 2)
    )
  }

  it("should specify discrete finite from above ordered set") {

    val discreteOrd1 = new DiscreteOrder.Finite.Above[Int, Int] { 
      override def successorOrNull(x: Int): Int | Null = if hasSuccessor(x) then x + 1 else null
      override def predecessorOrNull(x: Int): Int | Null = x - 1
      override def hasPredecessor(x: Int) = true
      override def upperBound: Int = 0
      override def compare(x: Int, y: Int): Int = intOrdering.compare(x, y)
    }

    validateDiscreteFiniteAboveOrder(
      discreteOrd1,
      0,
      List((-3, -2), (-2, -1), (-1, 0), (0, null), (1, null)),
      List(-2, -1, 0),
      List(1, 2, 3)
    )
  }

  it("should specify discrete finite ordered set") {

    val discreteOrd1 = new DiscreteOrder.Finite[Int, Int, Int] { 
      override def successorOrNull(x: Int): Int | Null = if hasSuccessor(x) then x + 1 else null
      override def predecessorOrNull(x: Int): Int | Null = if hasPredecessor(x) then x - 1 else null
      override def lowerBound: Int = 0
      override def upperBound: Int = 10
      override def compare(x: Int, y: Int): Int = intOrdering.compare(x, y)
    }

    validateDiscreteFiniteOrder(
      discreteOrd1,
      0,
      10,
      List((null, -1), (null, 0), (0, 1), (1, 2), (9, 10), (10, null), (11, null)),
      List(-3, -2, -1),
      List(0, 1, 2, 9, 10),
      List(11, 12, 13)
    )
  }
}

object DiscreteOrderSpec {

  def validateDiscreteFiniteOrder[E](
    discreteOrd: DiscreteOrder.Finite[E, E, E],
    lowerBound: E,
    upperBound: E,
    adjElements: List[AdjacentElements[E]],
    belowElements: List[E],
    setElements: List[E],
    aboveElements: List[E]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateDiscreteOrder(discreteOrd, adjElements)

    BoundedOrderSpec.validateBoundedOrder(
      discreteOrd, lowerBound, true, upperBound, true, belowElements, setElements, aboveElements
    )
  }

  def validateDiscreteFiniteBelowOrder[E](
    discreteOrd: DiscreteOrder.Finite.Below[E, E],
    lowerBound: E,
    adjElements: List[AdjacentElements[E]],
    belowElements: List[E],
    setElements: List[E]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateDiscreteOrder(discreteOrd, adjElements)

    BoundedOrderSpec.validateBoundedBelowOrder(
      discreteOrd, lowerBound, true, belowElements, setElements
    )
  }

  def validateDiscreteFiniteAboveOrder[E](
    discreteOrd: DiscreteOrder.Finite.Above[E, E],
    lowerBound: E,
    adjElements: List[AdjacentElements[E]],
    setElements: List[E],
    aboveElements: List[E]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateDiscreteOrder(discreteOrd, adjElements)

    BoundedOrderSpec.validateBoundedAboveOrder(
      discreteOrd, lowerBound, true, setElements, aboveElements
    )
  }

  def validateDiscreteOrder[E](
    discreteOrd: DiscreteOrder[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateDiscreteOrderSingle(discreteOrd, adjElements)
    validateDiscreteOrderSingle(discreteOrd.reversed, adjElements.map(_.reversed))
    validateDiscreteOrderSingle(discreteOrd.reversed.reversed, adjElements)
  }

  private def validateDiscreteOrderSingle[E](
    discreteOrd: DiscreteOrder[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    DiscreteSpec.validateDiscrete(discreteOrd, adjElements)

    val ordering = discreteOrd.toOrdering

    adjElements.foreach { e =>
      val p = e.predecessor
      val s = e.successor
      if s != null && p != null then {

        assert(discreteOrd.isAdjacent(p, s))
        assert(discreteOrd.isAdjacent(s, p))
        assert(discreteOrd.isSuccessor(p, s))

        assert(!discreteOrd.eqv(p, s))
        assert(!discreteOrd.eqv(s, p))

        assert(discreteOrd.neqv(p, s))
        assert(discreteOrd.neqv(s, p))

        assert(discreteOrd.gt(s, p))
        assert(discreteOrd.gteqv(s, p))
        assert(discreteOrd.compare(s, p) > 0)

        assert(discreteOrd.lt(p, s))
        assert(discreteOrd.lteqv(p, s))
        assert(discreteOrd.compare(p, s) < 0)

        assert(eq.eqv(discreteOrd.min(p, s), p))
        assert(eq.eqv(discreteOrd.min(s, p), p))

        assert(eq.eqv(discreteOrd.max(p, s), s))
        assert(eq.eqv(discreteOrd.max(s, p), s))

        assert(!ordering.equiv(p, s))
        assert(!ordering.equiv(s, p))

        assert(ordering.gt(s, p))
        assert(ordering.gteq(s, p))
        assert(ordering.compare(s, p) > 0)

        assert(ordering.lt(p, s))
        assert(ordering.lteq(p, s))
        assert(ordering.compare(p, s) < 0)

        assert(eq.eqv(ordering.min(p, s), p))
        assert(eq.eqv(ordering.min(s, p), p))

        assert(eq.eqv(ordering.max(p, s), s))
        assert(eq.eqv(ordering.max(s, p), s))
      }
    }
  }
}
