package ordset.tree.treap

import ordset.tree.core.Reduce

package object reduce {

  type TreapReduceFunc[K, V, C, R]  = Reduce.Func[K, V, Treap, C, R]

  type NodeReduceFunc[K, V, C, R]  = Reduce.Func[K, V, Treap.Node, C, R]
}
