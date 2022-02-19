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
      override def successorOrNone(x: Int): Discrete.Maybe[Int] = if hasSuccessor(x) then x + 1 else Discrete.None
      override def hasSuccessor(x: Int) = x < 10
    }

    validateDiscreteSucceeding(discrete1, List((1, 2), (9, 10), (11, Discrete.None), (12, Discrete.None)))

    val discrete2 = new Discrete.Succeeding.Infinite[Int] { 
      override def successor(x: Int): Int = x + 1
    }

    validateDiscreteSucceeding(discrete2, List((1, 2), (9, 10), (11, 12)))
  }

  it("should specify sequence of preceding elements") {

      val discrete1 = new Discrete.Preceding[Int] { 
        override def predecessorOrNone(x: Int): Discrete.Maybe[Int] = if hasPredecessor(x) then x - 1 else Discrete.None
        override def hasPredecessor(x: Int) = x > 0
      }

      validateDiscretePreceding(discrete1, List((Discrete.None, -1), (Discrete.None, 0), (0, 1), (10, 11)))

      val discrete2 = new Discrete.Preceding.Infinite[Int] { 
        override def predecessor(x: Int): Int = x - 1
      }

      validateDiscretePreceding(discrete2, List((-1, 0), (0, 1), (10, 11)))
  }

  it("should specify sequence of succeeding and preceding elements") {

    val discrete1 = new Discrete[Int] { 
      override def successorOrNone(x: Int): Discrete.Maybe[Int] = if hasSuccessor(x) then x + 1 else Discrete.None
      override def hasSuccessor(x: Int) = x < 10
      override def predecessorOrNone(x: Int): Discrete.Maybe[Int] = if hasPredecessor(x) then x - 1 else Discrete.None
      override def hasPredecessor(x: Int) = x > 0
    }

    validateDiscrete(
      discrete1, 
      List((Discrete.None, -1), (Discrete.None, 0), (0, 1), (1, 2), (9, 10), (11, Discrete.None), (12, Discrete.None))
    )

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
    validateDiscreteSucceedingSingle(discrete, adjElements)
    validateDiscretePrecedingSingle(discrete.reversed, adjElements.map(_.reversed))
    validateDiscreteSucceedingSingle(discrete.reversed.reversed, adjElements)
  }

  def validateDiscretePreceding[E](
    discrete: Discrete.Preceding[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateDiscretePrecedingSingle(discrete, adjElements)
    validateDiscreteSucceedingSingle(discrete.reversed, adjElements.map(_.reversed))
    validateDiscretePrecedingSingle(discrete.reversed.reversed, adjElements)
  }

  def validateDiscrete[E](
    discrete: Discrete[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateDiscreteSingle(discrete, adjElements)
    validateDiscreteSingle(discrete.reversed, adjElements.map(_.reversed))
    validateDiscreteSingle(discrete.reversed.reversed, adjElements)
  }

  def validateDiscreteSucceedingSingle[E](
    discrete: Discrete.Succeeding[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    adjElements.foreach { e =>
      e.predecessor match {
        case Discrete.None => {}
        case p: E @unchecked =>
          assert(eqvMaybe(e.successor, discrete.successorOrNone(p)))
          assert(eqvOption(e.successorOpt, discrete.successorOpt(p)))
          e.successor match {
            case Discrete.None => assert(!discrete.hasSuccessor(p))
            case s: E @unchecked => discrete match {
              case discrete: Discrete.Succeeding.Infinite[E] => 
                assert(eq.eqv(s, discrete.successor(p)))
              case _ => {}
            }
          }
      }
    }
  }

  def validateDiscretePrecedingSingle[E](
    discrete: Discrete.Preceding[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    adjElements.foreach { e =>
      e.successor match {
        case Discrete.None => {}
        case s: E @unchecked =>
          assert(eqvMaybe(e.predecessor, discrete.predecessorOrNone(s)))
          assert(eqvOption(e.predecessorOpt, discrete.predecessorOpt(s)))
          e.predecessor match {
            case Discrete.None => assert(!discrete.hasPredecessor(s))
            case p: E @unchecked => discrete match {
              case discrete: Discrete.Preceding.Infinite[E] => 
                assert(eq.eqv(p, discrete.predecessor(s)))
              case _ => {}
            }
          }
      }
    }
  }

  def validateDiscreteSingle[E](
    discrete: Discrete[E],
    adjElements: List[AdjacentElements[E]]
  )(
    implicit eq: Eq[E]
  ): Unit = {
    validateDiscreteSucceedingSingle(discrete, adjElements)
    validateDiscretePrecedingSingle(discrete, adjElements)
  }

  private def eqvMaybe[E](x: Discrete.Maybe[E], y : Discrete.Maybe[E])(implicit eq: Eq[E]): Boolean =
    (x, y) match {
      case (Discrete.None, Discrete.None) => true
      case (Discrete.None, _) => false
      case (_, Discrete.None) => false
      case(x: E @unchecked, y: E @unchecked) => eq.eqv(x, y)
    }

  private def eqvOption[E](x: Option[E], y : Option[E])(implicit eq: Eq[E]): Boolean =
    (x, y) match {
      case (None, None) => true
      case (None, _) => false
      case (_, None) => false
      case(Some(x), Some(y)) => eq.eqv(x, y)
    }
}
