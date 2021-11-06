package ordset.core.domain

import ordset.{Order, Hash, Show}
import ordset.core.SeqValidationPredicate
import ordset.util.label.Label
import ordset.core.domain.entity.DirectedLabeledEntity
import ordset.core.domain.OrderDirection.Reversed

/**
 * Order with assigned direction - ascending or descending.
 *
 * The main point of direction parameter is <u>to provide consistency</u> between different instances. 
 * For example, we can require two ascending orders to construct some new composed ascending order.
 * 
 * For some domains direction choice is trivial. For example, integers 1, 2, 3, ... follow in ascending order.
 * In other cases direction may be chosen arbitrary.
 *
 * @tparam E element type parameter
 * @tparam Dir direction type parameter
 */
trait DirectedOrder[E, +Dir <: OrderDir] extends Order[E] with Hash[E] with DirectedLabeledEntity[Dir] {

  /**
   * Validation function wrapping [[validateStrictly]] method.
   */
  final lazy val strictValidation: SeqValidationPredicate[E] = validateStrictly

  /**
   * Validation function wrapping [[validateNonStrictly]] method.
   */
  final lazy val nonStrictValidation: SeqValidationPredicate[E] = validateNonStrictly

  /**
   * Checks whether `x` and `y` follows strictly according to the given order.
   *
   * Examples:
   *
   * 1. Ascending order ([[isAscending]] == true):
   * {{{
   *   validateStrictly(3, 5) == true
   *   validateStrictly(3, 3) == false
   *   validateStrictly(5, 3) == false
   * }}}
   *
   * 2. Descending order ([[isAscending]] = false):
   * {{{
   *   validateStrictly(5, 3) == true
   *   validateStrictly(3, 3) == false
   *   validateStrictly(3, 5) == false
   * }}}
   */
  final def validateStrictly(x: E, y: E): Boolean =
    compare(x, y) == invertedSign

  /**
   * Checks whether `x` and `y` follows non-strictly according to the given order.
   *
   * Examples:
   *
   * 1. Ascending order ([[isAscending]] == true):
   * {{{
   *   validateStrictly(3, 5) == true
   *   validateStrictly(3, 3) == true
   *   validateStrictly(5, 3) == false
   * }}}
   *
   * 2. Descending order ([[isAscending]] = false):
   * {{{
   *   validateStrictly(5, 3) == true
   *   validateStrictly(3, 3) == true
   *   validateStrictly(3, 5) == false
   * }}}
   */
  final def validateNonStrictly(x: E, y: E): Boolean = {
    val cmp = compare(x, y)
    cmp == invertedSign || cmp == 0
  }

  override def toString: String = DirectedOrder.defaultShow.show(this)
}

object DirectedOrder {

  implicit def defaultHash[E]: Hash[DirectedOrder[E, _ <: OrderDir]] = DirectedLabeledEntity.defaultHash

  implicit lazy val defaultShow: Show[DirectedOrder[_, _ <: OrderDir]] =
    new DirectedLabeledEntity.DefaultShow(
      "DirectedOrder",
      Label.defaultSetShow,
      OrderDirection.defaultShow
    )

  /**
   * Returns reversed order such that
   * {{{
   *   if (order.isAscending == true) then reverse(order).isAscending == false
   *   if (order.isAscending == false) then reverse(order).isAscending == true
   * }}}
   * Output instance has flipped [[compare]] operation.
   */
  def reverse[E, Dir <: OrderDir](order: DirectedOrder[E, Dir]): DirectedOrder[E, Reversed[Dir]] = {
    val revDir = ValueOf(OrderDirection.reverse(order.direction))
    order match {
      case order: ReversedImpl[E, _] =>
        new DefaultImpl(order.labels, order.elementOrd, order.elementHash)(revDir)
      case order: DefaultImpl[E, _] => 
        new ReversedImpl(order.labels, order.elementOrd, order.elementHash)(revDir)
      case _ =>
        new ReversedImpl(order.labels, order, order)(revDir)
    }
  }

  /**
   * Base class for directed order.
   */
  abstract class Abstract[E, +Dir <: OrderDir](
    implicit dirValue: ValueOf[Dir]
  ) extends DirectedOrder[E, Dir] {

    final override val direction: Dir = dirValue.value
  }

  /**
   * Default implementation of order.
   */
  final class DefaultImpl[E, +Dir <: OrderDir](
    override val labels: Set[Label],
    val elementOrd: Order[E],
    val elementHash: Hash[E]
  )(
    implicit dirValue: ValueOf[Dir]
  ) extends Abstract[E, Dir] {

    override def compare(x: E, y: E): Int = elementOrd.compare(x, y)

    override def hash(x: E): Int = elementHash.hash(x)

    override def eqv(x: E, y: E): Boolean = elementHash.eqv(x, y)
  }

  /**
   * Reversed implementation of order. [[compare]] operation of input `elementOrd` is flipped .
   */
  final class ReversedImpl[E, +Dir <: OrderDir](
    override val labels: Set[Label],
    val elementOrd: Order[E],
    val elementHash: Hash[E]
  )(
    implicit dirValue: ValueOf[Dir]
  ) extends Abstract[E, Dir] {

    override def compare(x: E, y: E): Int = elementOrd.compare(y, x)

    override def hash(x: E): Int = elementHash.hash(x)

    override def eqv(x: E, y: E): Boolean = elementHash.eqv(y, x)
  }
}