package ordset.treap

import ordset.{Order, Show}

sealed trait Treap[K, Ord <: Order[K]] {

  def hasLeft: Boolean = false

  def hasRight: Boolean = false

  def isEmpty: Boolean = false
}

object Treap {

  implicit def treapShow[K, Ord <: Order[K]]: Show[Treap[K, Ord]] = Show.show(t => t.toString)

  sealed trait Node[K, Ord <: Order[K]] extends Treap[K, Ord] {

    val key: K
    val priority: Int

    override def toString: String = s"Treap.Node(key: $key)"
  }

  sealed trait NodeWithLeft[K, Ord <: Order[K]] extends Node[K, Ord] {

    override def hasLeft: Boolean = true

    val left: Node[K, Ord]
  }

  sealed trait NodeWithRight[K, Ord <: Order[K]] extends Node[K, Ord] {

    override def hasRight: Boolean = true

    val right: Node[K, Ord]
  }

  sealed case class Empty[K, Ord <: Order[K]]() extends Treap[K, Ord] {

    override def isEmpty: Boolean = true

    override def toString: String = s"Treap.Empty()"
  }

  sealed case class Leaf[K, Ord <: Order[K]](
    override val key: K,
    override val priority: Int,
  ) extends Node[K, Ord] {

    override def toString: String = s"Treap.Leaf(key: $key)"
  }

  sealed case class WithLeftOnly[K, Ord <: Order[K]](
    override val left: Node[K, Ord],
    override val key: K,
    override val priority: Int,
  ) extends NodeWithLeft[K, Ord] {

    override def toString: String = s"Treap.WithLeftOnly(key: $key)"
  }

  sealed case class WithRightOnly[K, Ord <: Order[K]](
    override val right: Node[K, Ord],
    override val key: K,
    override val priority: Int,
  ) extends NodeWithRight[K, Ord] {

    override def toString: String = s"Treap.WithRightOnly(key: $key)"
  }

  sealed case class WithLeftRight[K, Ord <: Order[K]](
    override val left: Node[K, Ord],
    override val right: Node[K, Ord],
    override val key: K,
    override val priority: Int,
  ) extends NodeWithLeft[K, Ord] with NodeWithRight[K, Ord] {

    override def toString: String = s"Treap.WithLeftRight(key: $key)"
  }
}