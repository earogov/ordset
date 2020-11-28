package ordset.tree.treap

import ordset.tree.core.Eval
import ordset.tree.core.eval.{TreeStack, TreeStackOps, TreeVisitStack}

package object eval {

  type TreapStack[K, V] = TreeStack.Stack[K, V, Treap]

  type NodeStack[K, V] = TreeStack.Stack[K, V, Treap.Node]

  type TreapStackOps[K, V, C] = TreeStackOps[K, V, Treap, C]

  type NodeStackOps[K, V, C] = TreeStackOps[K, V, Treap.Node, C]

  type TreapStackContext[K, V] = TreeStack.Context[K, V, Treap]

  type NodeStackContext[K, V] = TreeStack.Context[K, V, Treap.Node]

  type TreapVisitContext[K, V] = TreeVisitStack.Context[K, V, Treap]

  type NodeVisitContext[K, V] = TreeVisitStack.Context[K, V, Treap.Node]

  type TreapStackContextOps[K, V]  = TreeStack.ContextOps[K, V, Treap]

  type NodeStackContextOps[K, V]  = TreeStack.ContextOps[K, V, Treap.Node]

  type TreapVisitContextOps[K, V, C <: TreapVisitContext[K, V]]  = TreeVisitStack.ContextOps[K, V, Treap, C]

  type NodeVisitContextOps[K, V, C <: NodeVisitContext[K, V]]  = TreeVisitStack.ContextOps[K, V, Treap.Node, C]

  type TreapEvalFunc[K, V, C]  = Eval.BinaryFunc[K, V, Treap, C]

  type NodeEvalFunc[K, V, C]  = Eval.BinaryFunc[K, V, Treap.Node, C]
}
