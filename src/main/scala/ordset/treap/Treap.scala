package ordset.treap

import ordset.domain.Domain
import ordset.Show

sealed trait Treap[E, D <: Domain[E], W] {

  def hasLeft: Boolean = false

  def hasRight: Boolean = false

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
  }

  sealed trait NodeWithLeft[E, D <: Domain[E], W] extends Node[E, D, W] {

    val left: Node[E, D, W]

    override def hasLeft: Boolean = true
  }

  sealed trait NodeWithRight[E, D <: Domain[E], W] extends Node[E, D, W] {

    val right: Node[E, D, W]

    override def hasRight: Boolean = true
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

    override def toString: String = s"Treap.Leaf(key: $key, priority: $priority, value: $value)"
  }

  sealed case class NodeWithLeftOnly[E, D <: Domain[E], W](
    override val left: Node[E, D, W],
    override val key: E,
    override val priority: Int,
    override val value: W
  ) extends NodeWithLeft[E, D, W] {

    override def toString: String = s"Treap.NodeWithLeftOnly(key: $key, priority: $priority, value: $value)"
  }

  sealed case class NodeWithRightOnly[E, D <: Domain[E], W](
    override val right: Node[E, D, W],
    override val key: E,
    override val priority: Int,
    override val value: W
  ) extends NodeWithRight[E, D, W] {

    override def toString: String = s"Treap.NodeWithRightOnly(key: $key, priority: $priority, value: $value)"
  }

  sealed case class NodeWithLeftRight[E, D <: Domain[E], W](
    override val left: Node[E, D, W],
    override val right: Node[E, D, W],
    override val key: E,
    override val priority: Int,
    override val value: W
  ) extends NodeWithLeft[E, D, W] with NodeWithRight[E, D, W] {

    override def toString: String = s"Treap.NodeWithLeftRight(key: $key, priority: $priority, value: $value)"
  }
}