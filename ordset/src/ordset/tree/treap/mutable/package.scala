package ordset.tree.treap

import ordset.tree.core.{Eval, Fold}
import ordset.tree.core.eval.{TreeStack, TreeStackOps, TreeVisitStack}

package object mutable {

  // Tree stack
  type TreapStack[K, V] = TreeStack.Stack[K, V, MutableTreap]

  type NodeStack[K, V] = TreeStack.Stack[K, V, MutableTreap.Node]

  type TreapStackContext[K, V] = TreeStack.Context[K, V, MutableTreap]

  type NodeStackContext[K, V] = TreeStack.Context[K, V, MutableTreap.Node]

  type TreapStackContextOps[K, V]  = TreeStack.ContextOps[K, V, MutableTreap]

  type NodeStackContextOps[K, V]  = TreeStack.ContextOps[K, V, MutableTreap.Node]

  type TreapStackOps[K, V, C] = TreeStackOps[K, V, MutableTreap, C]

  type NodeStackOps[K, V, C] = TreeStackOps[K, V, MutableTreap.Node, C]

  // Tree visit stack
  type TreapVisitStack[K, V] = TreeVisitStack.Stack[K, V, MutableTreap]

  type NodeVisitStack[K, V] = TreeVisitStack.Stack[K, V, MutableTreap.Node]

  type TreapVisitContext[K, V] = TreeVisitStack.Context[K, V, MutableTreap]

  type NodeVisitContext[K, V] = TreeVisitStack.Context[K, V, MutableTreap.Node]

  type TreapVisitContextOps[K, V, C <: TreapVisitContext[K, V]]  = TreeVisitStack.ContextOps[K, V, MutableTreap, C]

  type NodeVisitContextOps[K, V, C <: NodeVisitContext[K, V]]  = TreeVisitStack.ContextOps[K, V, MutableTreap.Node, C]

  // Eval function
  type TreapEvalFunc[K, V, C]  = Eval.BinaryFunc[K, V, MutableTreap, C]

  type NodeEvalFunc[K, V, C]  = Eval.BinaryFunc[K, V, MutableTreap.Node, C]

  // Fold function
  type TreapFoldFunc[K, V, C, R]  = Fold.Func[K, V, MutableTreap, C, R]

  type NodeFoldFunc[K, V, C, R]  = Fold.Func[K, V, MutableTreap.Node, C, R]
}
