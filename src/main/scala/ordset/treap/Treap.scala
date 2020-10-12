package ordset.treap

import ordset.domain.Domain
import ordset.Show

sealed trait Treap[E, D <: Domain[E]] {

  def hasLeft: Boolean = false

  def hasRight: Boolean = false

  def isEmpty: Boolean = false
}

object Treap {

  implicit def treapShow[E, D <: Domain[E]]: Show[Treap[E, D]] = Show.show(t => t.toString)

  sealed trait Node[E, D <: Domain[E]] extends Treap[E, D] {

    val key: E
    val priority: Int

    override def toString: String = s"Treap.Node(key: $key)"
  }

  sealed trait NodeWithLeft[E, D <: Domain[E]] extends Node[E, D] {

    override def hasLeft: Boolean = true

    val left: Node[E, D]
  }

  sealed trait NodeWithRight[E, D <: Domain[E]] extends Node[E, D] {

    override def hasRight: Boolean = true

    val right: Node[E, D]
  }

  sealed case class Empty[E, D <: Domain[E]]() extends Treap[E, D] {

    override def isEmpty: Boolean = true

    override def toString: String = s"Treap.Empty()"
  }

  sealed case class Leaf[E, D <: Domain[E]](
    override val key: E,
    override val priority: Int,
  ) extends Node[E, D] {

    override def toString: String = s"Treap.Leaf(key: $key)"
  }

  sealed case class WithLeftOnly[E, D <: Domain[E]](
    override val left: Node[E, D],
    override val key: E,
    override val priority: Int,
  ) extends NodeWithLeft[E, D] {

    override def toString: String = s"Treap.WithLeftOnly(key: $key)"
  }

  sealed case class WithRightOnly[E, D <: Domain[E]](
    override val right: Node[E, D],
    override val key: E,
    override val priority: Int,
  ) extends NodeWithRight[E, D] {

    override def toString: String = s"Treap.WithRightOnly(key: $key)"
  }

  sealed case class WithLeftRight[E, D <: Domain[E]](
    override val left: Node[E, D],
    override val right: Node[E, D],
    override val key: E,
    override val priority: Int,
  ) extends NodeWithLeft[E, D] with NodeWithRight[E, D] {

    override def toString: String = s"Treap.WithLeftRight(key: $key)"
  }
}