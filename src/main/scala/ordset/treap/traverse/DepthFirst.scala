package ordset.treap.traverse

import ordset.treap.{Traverser, TraverserResult, TraverserVisit}

object DepthFirst {

  def makeLeftFirst[K]: Traverser[K] = LeftFirst.asInstanceOf[Traverser[K]]

  def makeRightFirst[K]: Traverser[K] = RightFirst.asInstanceOf[Traverser[K]]

  private val LeftFirst: Traverser[Any] = (_, visits) =>
    visits match {
      case TraverserVisit.None => TraverserResult.Left
      case TraverserVisit.Left => TraverserResult.Right
      case _ => TraverserResult.Up
    }

  private val RightFirst: Traverser[Any] = (_, visits) =>
    visits match {
      case TraverserVisit.None => TraverserResult.Right
      case TraverserVisit.Right => TraverserResult.Left
      case _ => TraverserResult.Up
    }
}
