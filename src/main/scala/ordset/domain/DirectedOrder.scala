package ordset.domain

import ordset._
import ordset.util.label.Label
import ordset.util.types.{>|<, SingleValue}

/**
 * Order with assigned direction - ascending or descending.
 *
 * For some domains direction choice is trivial. For example, integers 1, 2, 3, ... are follow in ascending order.
 * In other cases direction may be chosen arbitrary. The main point of trait is <u>to provide direction consistency</u>
 * between different order instances. For example, we can require two ascending orders to construct some new composed
 * ascending order.
 *
 * Direction is defined both with type parameter ([[DirectedOrder!.Dir]]) and value ([[DirectedOrder!.direction]]).
 *
 * @tparam E element type parameter
 * @tparam Dir direction type parameter
 */
trait DirectedOrder[E, +Dir <: OrderDir] extends Order[E] with Hash[E] {

  /**
   * Value representation of Dir type parameter.
   */
  def direction: Dir

  /**
   * Order label. Is used for equality checks and string representation of order instances.
   */
  def label: Label

  /**
   * Sign equals to
   * {{{
   *   1 if (isAscending == true)
   *   -1 if (isAscending == false)
   * }}}
   */
  final def sign: Int = if (isAscending) 1 else -1

  /**
   * Inverted sign equals to
   * {{{
   *   -1 if (isAscending == true)
   *   1 if (isAscending == false)
   * }}}
   */
  final def invertedSign: Int = if (isAscending) -1 else 1

  /**
   * Returns true if direction is ascending.
   */
  final def isAscending: Boolean = OrderDirection.isAscending(direction)

  /**
   * Returns true if direction is descending.
   */
  final def isDescending: Boolean = OrderDirection.isDescending(direction)

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

  /**
   * Returns reversed order such that
   * {{{
   *   if (this.isAscending == true) then this.reverse.isAscending == false
   *   if (this.isAscending == false) then this.reverse.isAscending == true
   * }}}
   * @tparam Rev direction type opposite to Dir
   */
  def reverse[Rev <: OrderDir](implicit ev: Dir >|< Rev): DirectedOrder[E, Rev] = {
    val revDir = OrderDirection.reverseTyped(direction)(ev)
    new DirectedOrder.ReversedWrapper[E, Rev](label, this, this)(SingleValue(revDir))
  }

  override def toString: String = DirectedOrder.defaultShow.show(this)
}

object DirectedOrder {

  implicit lazy val defaultHash: Hash[DirectedOrder[_, _ <: OrderDir]] =
    new DefaultHash(Label.defaultOrder, OrderDirection.defaultHash)

  implicit lazy val defaultShow: Show[DirectedOrder[_, _ <: OrderDir]] =
    new DefaultShow(Label.defaultShow, OrderDirection.defaultShow)

  /**
   * Base class for directed order.
   */
  abstract class Abstract[E, +Dir <: OrderDir](
    implicit dirValue: SingleValue[Dir]
  ) extends DirectedOrder[E, Dir] {

    final override val direction: Dir = dirValue.get
  }

  /**
   * Creates directed order from simple order instance.
   */
  final class Wrapper[E, +Dir <: OrderDir](
    override val label: Label,
    val elementOrd: Order[E],
    val elementHash: Hash[E]
  )(
    implicit dirValue: SingleValue[Dir]
  ) extends Abstract[E, Dir] {

    override def compare(x: E, y: E): Int = elementOrd.compare(x, y)

    override def hash(x: E): Int = elementHash.hash(x)

    override def eqv(x: E, y: E): Boolean = elementHash.eqv(x, y)

    override def reverse[Rev <: OrderDir](implicit ev: Dir >|< Rev): DirectedOrder[E, Rev] = {
      val revDir = OrderDirection.reverseTyped(direction)(ev)
      new ReversedWrapper[E, Rev](label, elementOrd, elementHash)(SingleValue(revDir))
    }
  }

  /**
   * Creates reversed directed order from simple order instance.
   */
  final class ReversedWrapper[E, +Dir <: OrderDir](
    override val label: Label,
    val elementOrd: Order[E],
    val elementHash: Hash[E]
  )(
    implicit dirValue: SingleValue[Dir]
  ) extends Abstract[E, Dir] {

    override def compare(x: E, y: E): Int = elementOrd.compare(y, x)

    override def hash(x: E): Int = elementHash.hash(x)

    override def eqv(x: E, y: E): Boolean = elementHash.eqv(y, x)

    override def reverse[Rev <: OrderDir](implicit ev: Dir >|< Rev): DirectedOrder[E, Rev] = {
      val revDir = OrderDirection.reverseTyped(direction)(ev)
      new Wrapper[E, Rev](label, elementOrd, elementHash)(SingleValue(revDir))
    }
  }

  final class DefaultHash(
    labelHash: Hash[Label],
    dirHash: Hash[OrderDir]
  ) extends Hash[DirectedOrder[_, _ <: OrderDir]] {

    import util.HashUtil._

    override def eqv(x: DirectedOrder[_, _ <: OrderDir], y: DirectedOrder[_, _ <: OrderDir]): Boolean =
      labelHash.eqv(x.label, y.label) && dirHash.eqv(x.direction, y.direction)

    override def hash(x: DirectedOrder[_, _ <: OrderDir]): Int =
      product2Hash(labelHash.hash(x.label), dirHash.hash(x.direction))
  }

  final class DefaultShow(
    labelShow: Show[Label],
    dirShow: Show[OrderDir]
  ) extends Show[DirectedOrder[_, _ <: OrderDir]] {

    override def show(t: DirectedOrder[_, _ <: OrderDir]): String =
      s"Order(label: ${labelShow.show(t.label)}, direction: ${dirShow.show(t.direction)})"
  }
}