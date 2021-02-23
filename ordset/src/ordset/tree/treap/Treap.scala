package ordset.tree.treap

import ordset.Order

trait Treap[+K, +V] {

  def hasLeft: Boolean = false

  def hasLeftInstance[KK >: K, VV >: V](tree: Treap[KK, VV]): Boolean = false

  def hasRight: Boolean = false

  def hasRightInstance[KK >: K, VV >: V](tree: Treap[KK, VV]): Boolean = false

  def isEmpty: Boolean = false

  def isNode: Boolean = false

  def isLeaf: Boolean = false
}

object Treap {

  trait Node[+K, +V] extends Treap[K, V] {

    def key: K

    def value: V

    def priority: Int = scala.util.hashing.byteswap32(key.hashCode())

    def getLeftOrNull: Node[K, V] = null

    def getRightOrNull: Node[K, V] = null
  }

  def nodePriorityOrder[K, V](implicit keyOrder: Order[K]): NodePriorityOrder[K, V] =
    new NodePriorityOrder(keyOrder)

  def nodePriorityCompare[K, V](
    priority1: Int,
    key1: K,
    priority2: Int,
    key2: K
  )(
    implicit keyOrder: Order[K]
  ): Int = {
    val cmp = intOrder.compare(priority1, priority2)
    if (cmp == 0) keyOrder.compare(key1, key2)
    else cmp
  }

  def nodePriorityEq[K, V](
    priority1: Int,
    key1: K,
    priority2: Int,
    key2: K
  )(
    implicit keyOrder: Order[K]
  ): Boolean = {
    val eq = intOrder.eqv(priority1, priority2)
    if (eq) keyOrder.eqv(key1, key2)
    else eq
  }

  final class NodePriorityOrder[K, V](
    val keyOrder: Order[K]
  ) extends Order[Node[K, V]] {

    override def compare(x: Node[K, V], y: Node[K, V]): Int =
      nodePriorityCompare(x.priority, x.key, y.priority, y.key)(keyOrder)

    override def eqv(x: Node[K, V], y: Node[K, V]): Boolean =
      nodePriorityEq(x.priority, x.key, y.priority, y.key)(keyOrder)
  }

  private val intOrder = ordset.instances.int.intOrder
}