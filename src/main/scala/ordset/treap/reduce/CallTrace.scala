package ordset.treap.reduce

import ordset.domain.Domain
import ordset.Show
import ordset.treap.{Reduce, Treap}

import scala.collection.immutable.Queue

object CallTrace {

  def toConsole[E, D <: Domain[E], C](implicit contextShow: Show[C]): Reduce.Func[E, D, C, Unit] =
    (tree, context, _) => println(makeString(tree, context, contextShow))

  def toQueue[E, D <: Domain[E], C](implicit contextShow: Show[C]): Reduce.Func[E, D, C, Queue[String]] =
    (tree, context, queue) => queue.appended(makeString(tree, context, contextShow))

  private def makeString[E, D <: Domain[E], C](tree: Treap[E, D], context: C, contextShow: Show[C]) =
    s"tree: $tree, context: ${contextShow.show(context)}"
}
