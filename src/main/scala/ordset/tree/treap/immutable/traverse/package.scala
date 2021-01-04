package ordset.tree.treap.immutable

import ordset.tree.core.{Navigation, Traverse}

package object traverse {

  type TreapTraverseFunc[K, V, C] = Traverse.BinaryFunc[K, V, ImmutableTreap, C]

  type NodeTraverseFunc[K, V, C] = Traverse.BinaryFunc[K, V, ImmutableTreap.Node, C]

  type TreapNavigationFunc[K, V, C] = Navigation.BinaryFunc[K, V, ImmutableTreap, C]

  type NodeNavigationFunc[K, V, C] = Navigation.BinaryFunc[K, V, ImmutableTreap.Node, C]

  type TreapTraverseOutput[K, V, C] = Traverse.BinaryOutput[K, V, ImmutableTreap, C]

  type NodeTraverseOutput[K, V, C] = Traverse.BinaryOutput[K, V, ImmutableTreap.Node, C]
}
