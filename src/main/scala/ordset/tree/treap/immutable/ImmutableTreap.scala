package ordset.tree.treap.immutable

import ordset.tree.treap.Treap
import ordset.Show

sealed trait ImmutableTreap[+K, +V] extends Treap[K, V]

object ImmutableTreap {

  implicit def treapShow[K, V]: Show[ImmutableTreap[K, V]] =
    ShowInstance.asInstanceOf[Show[ImmutableTreap[K, V]]]

  implicit def nodeShow[K, V]: Show[ImmutableTreap.Node[K, V]] =
    ShowInstance.asInstanceOf[Show[ImmutableTreap.Node[K, V]]]

  sealed trait Node[+K, +V] extends ImmutableTreap[K, V] with Treap.Node[K, V] {

    override def isNode: Boolean = true

    def withLeftTree[KK >: K, VV >: V](tree: ImmutableTreap[KK, VV]): Node[KK, VV] =
      tree match {
        case tree: Node[KK, VV] => withLeftNode(tree)
        case _ => withoutLeft()
      }

    def withLeftNode[KK >: K, VV >: V](node: Node[KK, VV]): NodeWithLeft[KK, VV]

    def withoutLeft(): Node[K, V]

    def withRightTree[KK >: K, VV >: V](tree: ImmutableTreap[KK, VV]): Node[KK, VV] =
      tree match {
        case tree: Node[KK, VV] => withRightNode(tree)
        case _ => withoutRight()
      }

    def withRightNode[KK >: K, VV >: V](node: Node[KK, VV]): NodeWithRight[KK, VV]

    def withoutRight(): Node[K, V]
  }

  sealed trait NodeWithLeft[+K, +V] extends Node[K, V] {

    val left: Node[K, V]

    override def hasLeft: Boolean = true

    override def hasLeftInstance[KK >: K, VV >: V](tree: Treap[KK, VV]): Boolean = left eq tree

    override def getLeftOrNull: Treap.Node[K, V] = left
  }

  sealed trait NodeWithRight[+K, +V] extends Node[K, V] {

    val right: Node[K, V]

    override def hasRight: Boolean = true

    override def hasRightInstance[KK >: K, VV >: V](tree: Treap[KK, VV]): Boolean = right eq tree

    override def getRightOrNull: Treap.Node[K, V] = right
  }

  final case object Empty extends ImmutableTreap[Nothing, Nothing] {

    override def isEmpty: Boolean = true

    override def toString: String = "ImmutableTreap.Empty"
  }

  final case class Leaf[+K, +V](
    override val key: K,
    override val priority: Int,
    override val value: V
  ) extends Node[K, V] {

    override def isLeaf: Boolean = true

    override def withLeftNode[KK >: K, VV >: V](node: Node[KK, VV]): NodeWithLeftOnly[KK, VV] =
      NodeWithLeftOnly(node, key, priority, value)

    override def withoutLeft(): Leaf[K, V] = this

    override def withRightNode[KK >: K, VV >: V](node: Node[KK, VV]): NodeWithRightOnly[KK, VV] =
      NodeWithRightOnly(node, key, priority, value)

    override def withoutRight(): Leaf[K, V] = this

    override def toString: String = s"ImmutableTreap.Leaf(key: $key, priority: $priority, value: $value)"
  }

  final case class NodeWithLeftOnly[+K, +V](
    override val left: Node[K, V],
    override val key: K,
    override val priority: Int,
    override val value: V
  ) extends NodeWithLeft[K, V] {

    override def withLeftNode[KK >: K, VV >: V](node: Node[KK, VV]): NodeWithLeftOnly[KK, VV] =
      if (hasLeftInstance(node)) this
      else NodeWithLeftOnly(node, key, priority, value)

    override def withoutLeft(): Leaf[K, V] =
      Leaf(key, priority, value)

    override def withRightNode[KK >: K, VV >: V](node: Node[KK, VV]): NodeWithLeftRight[KK, VV] =
      NodeWithLeftRight(left, node, key, priority, value)

    override def withoutRight(): NodeWithLeftOnly[K, V] = this

    override def toString: String = s"ImmutableTreap.NodeWithLeftOnly(key: $key, priority: $priority, value: $value)"
  }

  final case class NodeWithRightOnly[+K, +V](
    override val right: Node[K, V],
    override val key: K,
    override val priority: Int,
    override val value: V
  ) extends NodeWithRight[K, V] {

    override def withLeftNode[KK >: K, VV >: V](node: Node[KK, VV]): NodeWithLeftRight[KK, VV] =
      NodeWithLeftRight(node, right, key, priority, value)

    override def withoutLeft(): NodeWithRightOnly[K, V] = this

    override def withRightNode[KK >: K, VV >: V](node: Node[KK, VV]): NodeWithRightOnly[KK, VV] =
      if (hasRightInstance(node)) this
      else NodeWithRightOnly(node, key, priority, value)

    override def withoutRight(): Leaf[K, V] =
      Leaf(key, priority, value)

    override def toString: String = s"ImmutableTreap.NodeWithRightOnly(key: $key, priority: $priority, value: $value)"
  }

  final case class NodeWithLeftRight[+K, +V](
    override val left: Node[K, V],
    override val right: Node[K, V],
    override val key: K,
    override val priority: Int,
    override val value: V
  ) extends NodeWithLeft[K, V] with NodeWithRight[K, V] {

    override def withLeftNode[KK >: K, VV >: V](node: Node[KK, VV]): NodeWithLeftRight[KK, VV] =
      if (hasLeftInstance(node)) this
      else NodeWithLeftRight(node, right, key, priority, value)

    override def withoutLeft(): NodeWithRightOnly[K, V] =
      NodeWithRightOnly(right, key, priority, value)

    override def withRightNode[KK >: K, VV >: V](node: Node[KK, VV]): NodeWithLeftRight[KK, VV] =
      if (hasRightInstance(node)) this
      else NodeWithLeftRight(left, node, key, priority, value)

    override def withoutRight(): NodeWithLeftOnly[K, V] =
      NodeWithLeftOnly(left, key, priority, value)

    override def toString: String = s"ImmutableTreap.NodeWithLeftRight(key: $key, priority: $priority, value: $value)"
  }

  private lazy val ShowInstance: Show[ImmutableTreap[Any, Any]] = Show.fromToString
}