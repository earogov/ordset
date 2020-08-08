package ordset.treap.reduce

import ordset.{Order, Show}
import ordset.treap.{Reduce, Treap}

import scala.collection.immutable.Queue

object CallTrace {

  def toConsole[K, Ord <: Order[K], C](implicit contextShow: Show[C]): Reduce.Func[K, Ord, C, Unit] =
    (tree, context, _) => println(makeString(tree, context, contextShow))

  def toQueue[K, Ord <: Order[K], C](implicit contextShow: Show[C]): Reduce.Func[K, Ord, C, Queue[String]] =
    (tree, context, queue) => queue.appended(makeString(tree, context, contextShow))

  private def makeString[K, Ord <: Order[K], C](tree: Treap[K, Ord], context: C, contextShow: Show[C]) =
    s"tree: $tree, context: ${contextShow.show(context)}"
}
