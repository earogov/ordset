package ordset.tree.treap.mutable

import ordset.tree.treap.Treap

trait MutableTreap[K, V] extends Treap[K, V]

object MutableTreap {

  final case class Empty[K, V]() extends MutableTreap[K, V] {

    override def isEmpty: Boolean = true

    override def toString: String = "MutableTreap.Empty"
  }

  final class Node[K, V](
    override val key : K,
    override val value: V,
    override val priority: Int
  ) extends MutableTreap[K, V] with Treap.Node[K, V] {

    private var left: Treap.Node[K, V] | Null = _

    private var right: Treap.Node[K, V] | Null = _

    def setLeftTree(tree: Treap[K, V]): Unit =
      tree match {
        case tree: Treap.Node[K, V] => setLeftNode(tree)
        case _ => dropLeft()
      }

    def setLeftNode(node: Treap.Node[K, V] | Null): Unit = left = node

    def dropLeft(): Unit = left = null

    def setRightTree(tree: Treap[K, V]): Unit =
      tree match {
        case tree: Treap.Node[K, V] => setRightNode(tree)
        case _ => dropRight()
      }

    def setRightNode(node: Treap.Node[K, V] | Null): Unit = right = node

    def dropRight(): Unit = right = null

    override def getLeftOrNull: Treap.Node[K, V] | Null = left

    override def getRightOrNull: Treap.Node[K, V] | Null = right

    override def hasLeft: Boolean = left != null

    override def hasLeftInstance[KK >: K, VV >: V](tree: Treap[KK, VV]): Boolean = {
      val n = left
      if n != null then n eq tree else false
    }

    override def hasRight: Boolean = right != null

    override def hasRightInstance[KK >: K, VV >: V](tree: Treap[KK, VV]): Boolean = {
      val n = right
      if n != null then n eq tree else false
    }

    override def isEmpty: Boolean = false

    override def isNode: Boolean = true

    override def isLeaf: Boolean = left == null && right == null

    override def toString: String = s"MutableTreap.Node(key: $key, priority: $priority, value: $value)"
  }
}