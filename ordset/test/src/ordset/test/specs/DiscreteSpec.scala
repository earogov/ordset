package ordset.test.specs

import ordset.{Eq, Discrete}
import ordset.test.AdjacentElements

import org.scalatest.Assertions._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

@RunWith(classOf[JUnitRunner])
class DiscreteSpec extends AnyFunSpec {

  import DiscreteSpec._
  import ordset.givens.int._

  it("should specify sequence of succeeding elements") {

    val discrete1 = new Discrete.Succeeding[Int] { 
      override def successorOrNull(x: Int): Int | Null = if hasSuccessor(x) then x + 1 else null
      override def hasSuccessor(x: Int) = x < 10
    }

    validateDiscreteSucceeding(discrete1, List((1, 2), (9, 10), (11, null), (12, null)))

    val discrete2 = new Discrete.Succeeding.Infinite[Int] { 
      override def successor(x: Int): Int = x + 1
    }

    validateDiscreteSucceeding(discrete2, List((1, 2), (9, 10), (11, 12)))
  }

  it("should specify sequence of preceding elements") {

      val discrete1 = new Discrete.Preceding[Int] { 
        override def predecessorOrNull(x: Int): Int | Null = if hasPredecessor(x) then x - 1 else null
        override def hasPredecessor(x: Int) = x > 0
      }

      validateDiscretePreceding(discrete1, List((null, -1), (null, 0), (0, 1), (10, 11)))

      val discrete2 = new Discrete.Preceding.Infinite[Int] { 
        override def predecessor(x: Int): Int = x - 1
      }

      validateDiscretePreceding(discrete2, List((-1, 0), (0, 1), (10, 11)))
  }

  it("should specify sequence of succeeding and preceding elements") {

    val discrete1 = new Discrete[Int] { 
      override def successorOrNull(x: Int): Int | Null = if hasSuccessor(x) then x + 1 else null
      override def hasSuccessor(x: Int) = x < 10
      override def predecessorOrNull(x: Int): Int | Null = if hasPredecessor(x) then x - 1 else null
      override def hasPredecessor(x: Int) = x > 0
    }

    validateDiscrete(discrete1, List((null, -1), (null, 0), (0, 1), (1, 2), (9, 10), (11, null), (12, null)))

    val discrete2 = new Discrete.Infinite[Int] { 
      override def successor(x: Int): Int = x + 1
      override def predecessor(x: Int): Int = x - 1
    }

    validateDiscrete(discrete2, List((-1, 0), (0, 1), (1, 2), (9, 10), (11, 12)))
  }
}

object DiscreteSpec {

  def validateDiscreteSucceeding[E](
    discrete: Discrete.Succeeding[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateDiscreteSucceedingWithoutReversed(discrete, adjElements)
    validateDiscretePrecedingWithoutReversed(discrete.reversed, adjElements.map(_.reversed))
  }

  def validateDiscretePreceding[E](
    discrete: Discrete.Preceding[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateDiscretePrecedingWithoutReversed(discrete, adjElements)
    validateDiscreteSucceedingWithoutReversed(discrete.reversed, adjElements.map(_.reversed))
  }

  def validateDiscrete[E](
    discrete: Discrete[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateDiscreteWithoutReversed(discrete, adjElements)
    validateDiscreteWithoutReversed(discrete.reversed, adjElements.map(_.reversed))
  }

  def validateDiscreteSucceedingWithoutReversed[E](
    discrete: Discrete.Succeeding[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    adjElements.foreach { e =>
      if e.predecessor != null then {
        assert(eqvNullable(e.successor, discrete.successorOrNull(e.predecessor)))
        assert(eqvNullable(e.successor, discrete.successorOpt(e.predecessor).orNull))
        if e.successor == null then assert(!discrete.hasSuccessor(e.predecessor))
      }
    }
  }

  def validateDiscretePrecedingWithoutReversed[E](
    discrete: Discrete.Preceding[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    adjElements.foreach { e =>
      if e.successor != null then {
        assert(eqvNullable(e.predecessor, discrete.predecessorOrNull(e.successor)))
        assert(eqvNullable(e.predecessor, discrete.predecessorOpt(e.successor).orNull))
        if e.predecessor == null then assert(!discrete.hasPredecessor(e.successor))
      }
    }
  }

  def validateDiscreteWithoutReversed[E](
    discrete: Discrete[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateDiscreteSucceedingWithoutReversed(discrete, adjElements)
    validateDiscretePrecedingWithoutReversed(discrete, adjElements)
  }

  private def eqvNullable[E](x: E | Null, y : E | Null)(implicit eq: Eq[E]): Boolean =
    if x != null then
      if y != null then eq.eqv(x, y)
      else false
    else
      if y == null then true
      else false
}