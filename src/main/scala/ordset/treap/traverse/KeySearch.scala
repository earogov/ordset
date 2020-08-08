package ordset.treap.traverse

import ordset.Order
import ordset.treap.{TraverseStep, Treap}

object KeySearch {

//  def apply[K, Ord <: Order[K], C](key: K)(implicit ord: Ord): TraverseFuncStd[K, Ord, C] =
//    (tree, context) => {
//      val cmp = ord.compare(key, tree.key)
//      if (cmp > 0) tree match {
//        case n: Treap.NodeWithRight[K, Ord] => TraverseOutput(context, n.right, TraverseStep.Right, stop = false)
//        case _ => TraverseOutput(context, tree, TraverseStep.None, stop = true)
//      }
//      else if (cmp < 0)tree match {
//        case n: Treap.NodeWithLeft[K, Ord] => TraverseOutput(context, n.left, TraverseStep.Left, stop = false)
//        case _ => TraverseOutput(context, tree, TraverseStep.None, stop = true)
//      }
//      else TraverseOutput(context, tree, TraverseStep.None, stop = true)
//  }
}
