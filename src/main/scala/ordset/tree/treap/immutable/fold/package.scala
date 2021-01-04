package ordset.tree.treap.immutable

import ordset.tree.core.Fold

package object fold {

  type TreapFoldFunc[K, V, C, R]  = Fold.Func[K, V, ImmutableTreap, C, R]

  type NodeFoldFunc[K, V, C, R]  = Fold.Func[K, V, ImmutableTreap.Node, C, R]
}
