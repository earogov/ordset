package ordset.util.label

import ordset.util.label.Label.DefaultOrder
import ordset.{Hash, Order, Show}

import scala.collection.immutable.Queue

/**
 * Simple string wrapper with predefined order and equality.
 *
 * Methods `equals`, `hashCode`, `compareTo` are always consistent with [[DefaultOrder]].
 */
trait Label extends Comparable[Label] {

  def value: String

  final override def compareTo(o: Label): Int = DefaultOrder.compare(this, o)

  final override def equals(obj: Any): Boolean = obj match {
    case l: Label => DefaultOrder.eqv(this, l)
    case _ => false
  }

  final override def hashCode(): Int = DefaultOrder.hash(this)
}

object Label {

  def apply(value: String): Label = new DefaultImpl(value)

  implicit def defaultShow: Show[Label] = DefaultShow

  implicit def defaultOrder: Order[Label] = DefaultOrder

  implicit def defaultHash: Hash[Label] = DefaultOrder

  lazy val defaultSetHash: Hash[Set[Label]] = ordset.instances.set.setHash()

  lazy val defaultSetShow: Show[Set[Label]] = ordset.instances.set.setShow(defaultShow)

  def customSetShow(
    prefix: String,
    separator: String,
    suffix: String
  )(
    implicit labelShow: Show[Label]
  ): Show[Set[Label]] = {
    val iterableShow = new CustomIterableShow(prefix, separator, suffix)(labelShow)
    Show.show(iterableShow.show(_))
  }

  lazy val defaultQueueOrder: Order[Queue[Label]] = ordset.instances.queue.queueOrder

  lazy val defaultQueueHash: Hash[Queue[Label]] = ordset.instances.queue.queueHash

  lazy val defaultQueueShow: Show[Queue[Label]] = ordset.instances.queue.queueShow

  def customQueueShow(
    prefix: String,
    separator: String,
    suffix: String
  )(
    implicit labelShow: Show[Label]
  ): Show[Queue[Label]] = {
    val iterableShow = new CustomIterableShow(prefix, separator, suffix)(labelShow)
    Show.show(iterableShow.show(_))
  }

  // Typeclasses -------------------------------------------------------------- //
  object DefaultShow extends Show[Label] {

    import ordset.instances.string._

    override def show(t: Label): String = stringShow.show(t.value)
  }

  class CustomIterableShow(
    prefix: String,
    separator: String,
    suffix: String
  )(
    implicit labelShow: Show[Label]
  ) extends Show[Iterable[Label]] {

    override def show(t: Iterable[Label]): String = {
      var index = 1
      val stringBuilder = new StringBuilder()
      stringBuilder.append(prefix)
      t.foreach { label =>
        if (index > 1) stringBuilder.append(separator)
        stringBuilder.append(labelShow.show(label))
        index = index + 1
      }
      stringBuilder.append(suffix)
      stringBuilder.result()
    }
  }

  object DefaultOrder extends Order[Label] with Hash[Label] {

    import ordset.instances.string._
    import ordset.util.HashUtil._

    override lazy val toOrdering: Ordering[Label] = super.toOrdering

    override def compare(x: Label, y: Label): Int = stringOrder.compare(x.value, y.value)

    override def eqv(x: Label, y: Label): Boolean = stringOrder.eqv(x.value, y.value)

    override def hash(x: Label): Int = product1Hash(stringHash.hash(x.value))
  }

  // Private section ---------------------------------------------------------- //
  private final class DefaultImpl(override val value: String) extends Label
}
