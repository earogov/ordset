package ordset.tree.core.reduce

import ordset.Show
import ordset.tree.core.Reduce

import scala.collection.immutable.Queue

object CallTrace {

  def toConsole[K, V, Tree[KK, VV], C](
    implicit
    treeShow: Show[Tree[K, V]],
    contextShow: Show[C]
  ): Reduce.Func[K, V, Tree, C, Unit] =
    (tree, context, _) => println(makeString(tree, context, treeShow, contextShow))

  def toQueue[K, V, Tree[KK, VV], C](
    implicit
    treeShow: Show[Tree[K, V]],
    contextShow: Show[C]
  ): Reduce.Func[K, V, Tree, C, Queue[String]] =
    (tree, context, queue) => queue.appended(makeString(tree, context, treeShow, contextShow))

  // PRIVATE SECTION
  private def makeString[K, V, Tree[KK, VV], C](
    tree: Tree[K, V],
    context: C,
    treeShow: Show[Tree[K, V]],
    contextShow: Show[C]
  ): String =
    s"tree: ${treeShow.show(tree)}, context: ${contextShow.show(context)}"
}
