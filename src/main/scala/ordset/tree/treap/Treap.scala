package ordset.tree.treap

import ordset.{Order, Show}

sealed trait Treap[K, V] {

  def hasLeft: Boolean = false

  def hasLeftInstance(tree: Treap[K, V]): Boolean = false

  def hasRight: Boolean = false

  def hasRightInstance(tree: Treap[K, V]): Boolean = false

  def isEmpty: Boolean = false

  def isUniversal: Boolean = false

  def isNode: Boolean = false

  def isLeaf: Boolean = false
}

object Treap {

  implicit def treapShow[K, V]: Show[Treap[K, V]] =
    ShowInstance.asInstanceOf[Show[Treap[K, V]]]

  implicit def nodeShow[K, V]: Show[Treap.Node[K, V]] =
    ShowInstance.asInstanceOf[Show[Treap.Node[K, V]]]

  implicit def nodeOrder[K, V](implicit keyOrder: Order[K]): Order[Treap.Node[K, V]] =
    new NodePriorityOrder(ordset.instances.Int.intOrder, keyOrder)

  sealed trait WithValue[K, V] extends Treap[K, V] {

    val value: V
  }

  sealed trait Node[K, V] extends WithValue[K, V] {

    val key: K

    override val value: V

    val priority: Int

    override def isNode: Boolean = true

    def withLeftTree(tree: Treap[K, V]): Node[K, V] =
      tree match {
        case tree: Node[K, V] => withLeftNode(tree)
        case _ => withoutLeft()
      }

    def withLeftNode(node: Node[K, V]): NodeWithLeft[K, V]

    def withoutLeft(): Node[K, V]

    def withRightTree(tree: Treap[K, V]): Node[K, V] =
      tree match {
        case tree: Node[K, V] => withRightNode(tree)
        case _ => withoutRight()
      }

    def withRightNode(node: Node[K, V]): NodeWithRight[K, V]

    def withoutRight(): Node[K, V]
  }

  sealed trait NodeWithLeft[K, V] extends Node[K, V] {

    val left: Node[K, V]

    override def hasLeft: Boolean = true

    override def hasLeftInstance(tree: Treap[K, V]): Boolean = left eq tree
  }

  sealed trait NodeWithRight[K, V] extends Node[K, V] {

    val right: Node[K, V]

    override def hasRight: Boolean = true

    override def hasRightInstance(tree: Treap[K, V]): Boolean = right eq tree
  }

  sealed case class Empty[K, V]() extends Treap[K, V] {

    override def isEmpty: Boolean = true

    override def toString: String = "Treap.Empty()"
  }

  sealed case class Universal[K, V](
    override val value: V
  ) extends WithValue[K, V] {

    override def isUniversal: Boolean = true

    override def toString: String = s"Treap.Universal(value: $value)"
  }

  sealed case class Leaf[K, V](
    override val key: K,
    override val priority: Int,
    override val value: V
  ) extends Node[K, V] {

    override def isLeaf: Boolean = true

    override def withLeftNode(node: Node[K, V]): NodeWithLeftOnly[K, V] =
      NodeWithLeftOnly(node, key, priority, value)

    override def withoutLeft(): Leaf[K, V] = this

    override def withRightNode(node: Node[K, V]): NodeWithRightOnly[K, V] =
      NodeWithRightOnly(node, key, priority, value)

    override def withoutRight(): Leaf[K, V] = this

    override def toString: String = s"Treap.Leaf(key: $key, priority: $priority, value: $value)"
  }

  sealed case class NodeWithLeftOnly[K, V](
    override val left: Node[K, V],
    override val key: K,
    override val priority: Int,
    override val value: V
  ) extends NodeWithLeft[K, V] {

    override def withLeftNode(node: Node[K, V]): NodeWithLeftOnly[K, V] =
      if (hasLeftInstance(node)) this
      else NodeWithLeftOnly(node, key, priority, value)

    override def withoutLeft(): Leaf[K, V] =
      Leaf(key, priority, value)

    override def withRightNode(node: Node[K, V]): NodeWithLeftRight[K, V] =
      NodeWithLeftRight(left, node, key, priority, value)

    override def withoutRight(): NodeWithLeftOnly[K, V] = this

    override def toString: String = s"Treap.NodeWithLeftOnly(key: $key, priority: $priority, value: $value)"
  }

  sealed case class NodeWithRightOnly[K, V](
    override val right: Node[K, V],
    override val key: K,
    override val priority: Int,
    override val value: V
  ) extends NodeWithRight[K, V] {

    override def withLeftNode(node: Node[K, V]): NodeWithLeftRight[K, V] =
      NodeWithLeftRight(node, right, key, priority, value)

    override def withoutLeft(): NodeWithRightOnly[K, V] = this

    override def withRightNode(node: Node[K, V]): NodeWithRightOnly[K, V] =
      if (hasRightInstance(node)) this
      else NodeWithRightOnly(node, key, priority, value)

    override def withoutRight(): Leaf[K, V] =
      Leaf(key, priority, value)

    override def toString: String = s"Treap.NodeWithRightOnly(key: $key, priority: $priority, value: $value)"
  }

  sealed case class NodeWithLeftRight[K, V](
    override val left: Node[K, V],
    override val right: Node[K, V],
    override val key: K,
    override val priority: Int,
    override val value: V
  ) extends NodeWithLeft[K, V] with NodeWithRight[K, V] {

    override def withLeftNode(node: Node[K, V]): NodeWithLeftRight[K, V] =
      if (hasLeftInstance(node)) this
      else NodeWithLeftRight(node, right, key, priority, value)

    override def withoutLeft(): NodeWithRightOnly[K, V] =
      NodeWithRightOnly(right, key, priority, value)

    override def withRightNode(node: Node[K, V]): NodeWithLeftRight[K, V] =
      if (hasRightInstance(node)) this
      else NodeWithLeftRight(left, node, key, priority, value)

    override def withoutRight(): NodeWithLeftOnly[K, V] =
      NodeWithLeftOnly(left, key, priority, value)

    override def toString: String = s"Treap.NodeWithLeftRight(key: $key, priority: $priority, value: $value)"
  }

  final class NodePriorityOrder[K, V](
    val intOrder: Order[Int],
    val keyOrder: Order[K]
  ) extends Order[Node[K, V]] {

    override def compare(x: Node[K, V], y: Node[K, V]): Int = {
      val cmp = intOrder.compare(x.priority, y.priority)
      if (cmp == 0) keyOrder.compare(x.key, y.key)
      else cmp
    }

    override def eqv(x: Node[K, V], y: Node[K, V]): Boolean = {
      val eq = intOrder.eqv(x.priority, y.priority)
      if (eq) keyOrder.eqv(x.key, y.key)
      else eq
    }
  }

  private lazy val ShowInstance: Show[Treap[Any, Any]] = Show.fromToString
}