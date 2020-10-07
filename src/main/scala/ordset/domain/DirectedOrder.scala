package ordset.domain

import ordset._
import ordset.util.label.Label
import ordset.util.types.{>|<, SingleValue}

trait DirectedOrder[E, +Dir <: OrderDir] extends Order[E] with Hash[E] {

  def direction: Dir

  def label: Label

  def sign: Int = if (isAscending) 1 else -1

  def invertedSign: Int = if (isAscending) -1 else 1

  def isAscending: Boolean = OrderDirection.isAscending(direction)

  def isDescending: Boolean = OrderDirection.isDescending(direction)

  def reverse[Rev <: OrderDir](implicit ev: Dir >|< Rev): DirectedOrder[E, Rev] = {
    val revDir = OrderDirection.reverseTyped(direction)(ev)
    new DirectedOrder.ReversedWrapper[E, Rev](label, this, this)(SingleValue(revDir))
  }

  override def toString: String = DirectedOrder.defaultShow.show(this)
}

object DirectedOrder {

  implicit def defaultHash[E, Dir <: OrderDir]: Hash[DirectedOrder[E, Dir]] =
    defaultHashInstance.asInstanceOf[Hash[DirectedOrder[E, Dir]]]

  implicit def defaultShow[E, Dir <: OrderDir]: Show[DirectedOrder[E, Dir]] =
    defaultShowInstance.asInstanceOf[Show[DirectedOrder[E, Dir]]]

  abstract class Abstract[E, +Dir <: OrderDir](
    implicit dirValue: SingleValue[Dir]
  ) extends DirectedOrder[E, Dir] {

    final override val direction: Dir = dirValue.get

    final override val sign: Int = super.sign

    final override val invertedSign: Int = super.invertedSign

    final override val isAscending: Boolean = super.isAscending

    final override val isDescending: Boolean = super.isDescending
  }

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

  final class ReversedWrapper[E, +Dir <: OrderDir](
    override val label: Label,
    val elementOrd: Order[E],
    val elementHash: Hash[E]
  )(
    implicit dirValue: SingleValue[Dir]
  ) extends Abstract[E, Dir] {

    override def compare(x: E, y: E): Int = - elementOrd.compare(x, y)

    override def hash(x: E): Int = elementHash.hash(x)

    override def eqv(x: E, y: E): Boolean = elementHash.eqv(x, y)

    override def reverse[Rev <: OrderDir](implicit ev: Dir >|< Rev): DirectedOrder[E, Rev] = {
      val revDir = OrderDirection.reverseTyped(direction)(ev)
      new Wrapper[E, Rev](label, elementOrd, elementHash)(SingleValue(revDir))
    }
  }

  final class DefaultHash[E](
    labelHash: Hash[Label],
    dirHash: Hash[OrderDir]
  ) extends Hash[DirectedOrder[E, OrderDir]] {

    import util.Hash._

    override def eqv(x: DirectedOrder[E, OrderDir], y: DirectedOrder[E, OrderDir]): Boolean =
      labelHash.eqv(x.label, y.label) && dirHash.eqv(x.direction, y.direction)

    override def hash(x: DirectedOrder[E, OrderDir]): Int =
      product2Hash(labelHash.hash(x.label), dirHash.hash(x.direction))
  }

  final class DefaultShow[E](
    labelShow: Show[Label],
    dirShow: Show[OrderDir]
  ) extends Show[DirectedOrder[E, OrderDir]] {

    override def show(t: DirectedOrder[E, OrderDir]): String =
      s"Order(label: ${labelShow.show(t.label)}, direction: ${dirShow.show(t.direction)})"
  }

  private lazy val defaultHashInstance: Hash[DirectedOrder[Any, OrderDir]] =
    new DefaultHash[Any](Label.defaultOrder, OrderDirection.defaultHash)

  private lazy val defaultShowInstance: Show[DirectedOrder[Any, OrderDir]] =
    new DefaultShow[Any](Label.defaultShow, OrderDirection.defaultShow)
}