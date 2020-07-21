package ordset.treap.reduce

import ordset.treap.Treap.Node
import ordset.treap.{Reducer, TraverserResult, TraverserVisit}

import scala.collection.immutable.Queue

object CallTrace {

  def toConsole[K]: Reducer[K, Unit] = ConsolePrinter.asInstanceOf[Reducer[K, Unit]]

  def toQueue[K]: Reducer[K, Queue[String]] = QueueCollector.asInstanceOf[Reducer[K, Queue[String]]]

  private val ConsolePrinter: Reducer[Any, Unit] = (node, visits, next, _) =>
    println(makeString(node, visits, next))

  private val QueueCollector: Reducer[Any, Queue[String]] = (node, visits, next, queue) =>
    queue.appended(makeString(node, visits, next))

  private def makeString(node: Node[_, _], visits: TraverserVisit.Type, next: TraverserResult.Type) =
    s"node: $node, visits: ${TraverserVisit.toString(visits)}, next: ${TraverserResult.toString(next)}"
}
