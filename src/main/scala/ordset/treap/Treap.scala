package ordset.treap

import ordset.domain.Domain
import ordset.Show

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

  implicit def treapShow[E, D <: Domain[E], W]: Show[Treap[E, D, W]] = Show.show(t => t.toString)

  sealed trait Node[E, D <: Domain[E], W] extends Treap[E, D, W] {

    val key: E
    val value: W
    val priority: Int

    override def isNode: Boolean = true

    def withLeft(tree: Node[E, D, W]): NodeWithLeft[E, D, W]

    def withoutLeft(): Node[E, D, W]

    def withRight(tree: Node[E, D, W]): NodeWithRight[E, D, W]

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

    override def withLeft(tree: Node[E, D, W]): NodeWithLeftOnly[E, D, W] =
      NodeWithLeftOnly(tree, key, priority, value)

    override def withoutLeft(): Leaf[E, D, W] = this

    override def withRight(tree: Node[E, D, W]): NodeWithRightOnly[E, D, W] =
      NodeWithRightOnly(tree, key, priority, value)

    override def withoutRight(): Leaf[E, D, W] = this

    override def toString: String = s"Treap.Leaf(key: $key, priority: $priority, value: $value)"
  }

  sealed case class NodeWithLeftOnly[E, D <: Domain[E], W](
    override val left: Node[E, D, W],
    override val key: E,
    override val priority: Int,
    override val value: W
  ) extends NodeWithLeft[E, D, W] {

    override def withLeft(tree: Node[E, D, W]): NodeWithLeftOnly[E, D, W] =
      if (hasLeftInstance(tree)) this
      else NodeWithLeftOnly(tree, key, priority, value)

    override def withoutLeft(): Leaf[E, D, W] =
      Leaf(key, priority, value)

    override def withRight(tree: Node[E, D, W]): NodeWithLeftRight[E, D, W] =
      NodeWithLeftRight(left, tree, key, priority, value)

    override def withoutRight(): NodeWithLeftOnly[E, D, W] = this

    override def toString: String = s"Treap.NodeWithLeftOnly(key: $key, priority: $priority, value: $value)"
  }

  sealed case class NodeWithRightOnly[E, D <: Domain[E], W](
    override val right: Node[E, D, W],
    override val key: E,
    override val priority: Int,
    override val value: W
  ) extends NodeWithRight[E, D, W] {

    override def withLeft(tree: Node[E, D, W]): NodeWithLeftRight[E, D, W] =
      NodeWithLeftRight(tree, right, key, priority, value)

    override def withoutLeft(): NodeWithRightOnly[E, D, W] = this

    override def withRight(tree: Node[E, D, W]): NodeWithRightOnly[E, D, W] =
      if (hasRightInstance(tree)) this
      else NodeWithRightOnly(tree, key, priority, value)

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

    override def withLeft(tree: Node[E, D, W]): NodeWithLeftRight[E, D, W] =
      if (hasLeftInstance(tree)) this
      else NodeWithLeftRight(tree, right, key, priority, value)

    override def withoutLeft(): NodeWithRightOnly[E, D, W] =
      NodeWithRightOnly(right, key, priority, value)

    override def withRight(tree: Node[E, D, W]): NodeWithLeftRight[E, D, W] =
      if (hasRightInstance(tree)) this
      else NodeWithLeftRight(left, tree, key, priority, value)

    override def withoutRight(): NodeWithLeftOnly[E, D, W] =
      NodeWithLeftOnly(left, key, priority, value)

    override def toString: String = s"Treap.NodeWithLeftRight(key: $key, priority: $priority, value: $value)"
  }
}