package ordset.treap

import ordset.domain.Domain
import ordset.{Order, Show}

sealed trait Treap[E, D <: Domain[E], W] {

  def hasLeft: Boolean = false

  def hasLeftInstance(tree: Treap[E, D, W]): Boolean = false

  def hasRight: Boolean = false

  def hasRightInstance(tree: Treap[E, D, W]): Boolean = false

  def isEmpty: Boolean = false

  def isUniversal: Boolean = false

  def isNode: Boolean = false
}

object Treap {

  implicit def treapShow[E, D <: Domain[E], W]: Show[Treap[E, D, W]] =
    ShowInstance.asInstanceOf[Show[Treap[E, D, W]]]

  implicit def nodeShow[E, D <: Domain[E], W]: Show[Treap.Node[E, D, W]] =
    ShowInstance.asInstanceOf[Show[Treap.Node[E, D, W]]]

  implicit def nodeOrder[E, D <: Domain[E], W](implicit keyOrder: Order[E]): Order[Treap.Node[E, D, W]] =
    new NodePriorityOrder(ordset.instances.Int.intOrder, keyOrder)

  sealed trait Node[E, D <: Domain[E], W] extends Treap[E, D, W] {

    val key: E
    val value: W
    val priority: Int

    override def isNode: Boolean = true

    def withLeftTree(tree: Treap[E, D, W]): Node[E, D, W] =
      tree match {
        case tree: Node[E, D, W] => withLeftNode(tree)
        case _ => withoutLeft()
      }

    def withLeftNode(node: Node[E, D, W]): NodeWithLeft[E, D, W]

    def withoutLeft(): Node[E, D, W]

    def withRightTree(tree: Treap[E, D, W]): Node[E, D, W] =
      tree match {
        case tree: Node[E, D, W] => withRightNode(tree)
        case _ => withoutRight()
      }

    def withRightNode(node: Node[E, D, W]): NodeWithRight[E, D, W]

    def withoutRight(): Node[E, D, W]
  }

  sealed trait NodeWithLeft[E, D <: Domain[E], W] extends Node[E, D, W] {

    val left: Node[E, D, W]

    override def hasLeft: Boolean = true

    override def hasLeftInstance(tree: Treap[E, D, W]): Boolean = left eq tree
  }

  sealed trait NodeWithRight[E, D <: Domain[E], W] extends Node[E, D, W] {

    val right: Node[E, D, W]

    override def hasRight: Boolean = true

    override def hasRightInstance(tree: Treap[E, D, W]): Boolean = right eq tree
  }

  sealed case class Empty[E, D <: Domain[E], W]() extends Treap[E, D, W] {

    override def isEmpty: Boolean = true

    override def toString: String = "Treap.Empty()"
  }

  sealed case class Universal[E, D <: Domain[E], W](
    value: W
  ) extends Treap[E, D, W] {

    override def isUniversal: Boolean = true

    override def toString: String = s"Treap.Universal(value: $value)"
  }

  sealed case class Leaf[E, D <: Domain[E], W](
    override val key: E,
    override val priority: Int,
    override val value: W
  ) extends Node[E, D, W] {

    override def withLeftNode(node: Node[E, D, W]): NodeWithLeftOnly[E, D, W] =
      NodeWithLeftOnly(node, key, priority, value)

    override def withoutLeft(): Leaf[E, D, W] = this

    override def withRightNode(node: Node[E, D, W]): NodeWithRightOnly[E, D, W] =
      NodeWithRightOnly(node, key, priority, value)

    override def withoutRight(): Leaf[E, D, W] = this

    override def toString: String = s"Treap.Leaf(key: $key, priority: $priority, value: $value)"
  }

  sealed case class NodeWithLeftOnly[E, D <: Domain[E], W](
    override val left: Node[E, D, W],
    override val key: E,
    override val priority: Int,
    override val value: W
  ) extends NodeWithLeft[E, D, W] {

    override def withLeftNode(node: Node[E, D, W]): NodeWithLeftOnly[E, D, W] =
      if (hasLeftInstance(node)) this
      else NodeWithLeftOnly(node, key, priority, value)

    override def withoutLeft(): Leaf[E, D, W] =
      Leaf(key, priority, value)

    override def withRightNode(node: Node[E, D, W]): NodeWithLeftRight[E, D, W] =
      NodeWithLeftRight(left, node, key, priority, value)

    override def withoutRight(): NodeWithLeftOnly[E, D, W] = this

    override def toString: String = s"Treap.NodeWithLeftOnly(key: $key, priority: $priority, value: $value)"
  }

  sealed case class NodeWithRightOnly[E, D <: Domain[E], W](
    override val right: Node[E, D, W],
    override val key: E,
    override val priority: Int,
    override val value: W
  ) extends NodeWithRight[E, D, W] {

    override def withLeftNode(node: Node[E, D, W]): NodeWithLeftRight[E, D, W] =
      NodeWithLeftRight(node, right, key, priority, value)

    override def withoutLeft(): NodeWithRightOnly[E, D, W] = this

    override def withRightNode(node: Node[E, D, W]): NodeWithRightOnly[E, D, W] =
      if (hasRightInstance(node)) this
      else NodeWithRightOnly(node, key, priority, value)

    override def withoutRight(): Leaf[E, D, W] =
      Leaf(key, priority, value)

    override def toString: String = s"Treap.NodeWithRightOnly(key: $key, priority: $priority, value: $value)"
  }

  sealed case class NodeWithLeftRight[E, D <: Domain[E], W](
    override val left: Node[E, D, W],
    override val right: Node[E, D, W],
    override val key: E,
    override val priority: Int,
    override val value: W
  ) extends NodeWithLeft[E, D, W] with NodeWithRight[E, D, W] {

    override def withLeftNode(node: Node[E, D, W]): NodeWithLeftRight[E, D, W] =
      if (hasLeftInstance(node)) this
      else NodeWithLeftRight(node, right, key, priority, value)

    override def withoutLeft(): NodeWithRightOnly[E, D, W] =
      NodeWithRightOnly(right, key, priority, value)

    override def withRightNode(node: Node[E, D, W]): NodeWithLeftRight[E, D, W] =
      if (hasRightInstance(node)) this
      else NodeWithLeftRight(left, node, key, priority, value)

    override def withoutRight(): NodeWithLeftOnly[E, D, W] =
      NodeWithLeftOnly(left, key, priority, value)

    override def toString: String = s"Treap.NodeWithLeftRight(key: $key, priority: $priority, value: $value)"
  }

  final class NodePriorityOrder[E, D <: Domain[E], W](
    val intOrder: Order[Int],
    val keyOrder: Order[E]
  ) extends Order[Node[E, D, W]] {

    override def compare(x: Node[E, D, W], y: Node[E, D, W]): Int = {
      val cmp = intOrder.compare(x.priority, y.priority)
      if (cmp == 0) keyOrder.compare(x.key, y.key)
      else cmp
    }

    override def eqv(x: Node[E, D, W], y: Node[E, D, W]): Boolean = {
      val eq = intOrder.eqv(x.priority, y.priority)
      if (eq) keyOrder.eqv(x.key, y.key)
      else eq
    }
  }

  private lazy val ShowInstance: Show[Treap[Any, Domain[Any], Any]] = Show.fromToString
}