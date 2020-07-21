package ordset.treap.traverse

import ordset.Order
import ordset.treap.{Traverser, TraverserResult}

object KeySearch {

  def apply[K](key: K)(implicit ord: Order[K]): Traverser[K] = (node, _) => {
    val cmp = ord.compare(key, node.key)
    if (cmp > 0)
      if (node.hasRight) TraverserResult.Right
      else TraverserResult.Stop
    else if (cmp < 0)
      if (node.hasLeft) TraverserResult.Left
      else TraverserResult.Stop
    else
      TraverserResult.Stop
  }
}
