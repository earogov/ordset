package ordset.treap.reduce

import ordset.domain.Domain
import ordset.Show
import ordset.treap.{Reduce, Treap}

import scala.collection.immutable.Queue

object CallTrace {

  def toConsole[E, D <: Domain[E], W, C](
    implicit
    treeShow: Show[Treap[E, D, W]],
    contextShow: Show[C]
  ): Reduce.Func[E, D, W, C, Unit] =
    (tree, context, _) => println(makeString(tree, context, treeShow, contextShow))

  def toQueue[E, D <: Domain[E], W, C](
    implicit
    treeShow: Show[Treap[E, D, W]],
    contextShow: Show[C]
  ): Reduce.Func[E, D, W, C, Queue[String]] =
    (tree, context, queue) => queue.appended(makeString(tree, context, treeShow, contextShow))

  private def makeString[E, D <: Domain[E], W, C](
    tree: Treap[E, D, W],
    context: C, treeShow: Show[Treap[E, D, W]],
    contextShow: Show[C]
  ): String =
    s"tree: ${treeShow.show(tree)}, context: ${contextShow.show(context)}"
}
