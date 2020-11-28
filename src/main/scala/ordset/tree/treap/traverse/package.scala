package ordset.tree.treap

import ordset.tree.core.{Navigate, Traverse}

package object traverse {

  type TreapTraverseFunc[K, V, C] = Traverse.BinaryFunc[K, V, Treap, C]

  type NodeTraverseFunc[K, V, C] = Traverse.BinaryFunc[K, V, Treap.Node, C]

  type TreapNavigateFunc[K, V, C] = Navigate.BinaryFunc[K, V, Treap, C]

  type NodeNavigateFunc[K, V, C] = Navigate.BinaryFunc[K, V, Treap.Node, C]

  type TreapTraverseOutput[K, V, C] = Traverse.BinaryOutput[K, V, Treap, C]

  type NodeTraverseOutput[K, V, C] = Traverse.BinaryOutput[K, V, Treap.Node, C]
}
