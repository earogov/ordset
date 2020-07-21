package ordset.treap

import ordset.Order

import scala.collection.mutable

sealed trait Treap[K, Ord <: Order[K]] {

  def hasLeft: Boolean = false

  def hasRight: Boolean = false

  def isEmpty: Boolean = false

  def reduce[R](reducer: Reducer[K, R], traverser: Traverser[K], initial: R): R
}

object Treap {

  sealed trait Node[K, Ord <: Order[K]] extends Treap[K, Ord] {

    val key: K
    val priority: Int

    override def reduce[R](reducer: Reducer[K, R], traverser: Traverser[K], initial: R): R = {
      var node = this
      var visit = TraverserVisit.None
      var next = traverser(node, visit)
      var result = reducer(node, visit, next, initial)
      val stack = new mutable.Stack[(Node[K, Ord], TraverserVisit.Type)](8)
      stack.push((node, visit))
      do {
        next match {
          case TraverserResult.Left => node match {
            case l: NodeWithLeft[K, Ord] =>
              stack.push((node, TraverserVisit.addLeftVisit(visit)))
              node = l.left
              visit = TraverserVisit.None
              next = traverser(node, visit)
              result = reducer(node, visit, next, result)
            case _ =>
              visit = TraverserVisit.addLeftVisit(visit)
              next = traverser(node, visit)
              result = reducer(node, visit, next, result)
          }
          case TraverserResult.Right => node match {
            case r: NodeWithRight[K, Ord] =>
              stack.push((node, TraverserVisit.addRightVisit(visit)))
              node = r.right
              visit = TraverserVisit.None
              next = traverser(node, visit)
              result = reducer(node, visit, next, result)
            case _ =>
              visit = TraverserVisit.addRightVisit(visit)
              next = traverser(node, visit)
              result = reducer(node, visit, next, result)
          }
          case TraverserResult.Up =>
            if (stack.nonEmpty) {
              val state = stack.pop()
              node = state._1
              visit = state._2
              next = traverser(node, visit)
              if (stack.nonEmpty)
                result = reducer(node, visit, next, result)
            }
          case TraverserResult.Stop =>
            stack.clear()
        }
      } while (stack.nonEmpty)
      result
    }

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

    override def reduce[R](reducer: Reducer[K, R], traverser: Traverser[K], initial: R): R = initial

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