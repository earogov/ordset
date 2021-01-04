package ordset.tree.treap.immutable

import ordset.tree.core.Eval
import ordset.tree.core.eval.{TreeStack, TreeStackOps, TreeVisitStack}

package object eval {

  // Tree stack
  type TreapStack[K, V] = TreeStack.Stack[K, V, ImmutableTreap]

  type NodeStack[K, V] = TreeStack.Stack[K, V, ImmutableTreap.Node]

  type TreapStackContext[K, V] = TreeStack.Context[K, V, ImmutableTreap]

  type NodeStackContext[K, V] = TreeStack.Context[K, V, ImmutableTreap.Node]

  type TreapStackContextOps[K, V]  = TreeStack.ContextOps[K, V, ImmutableTreap]

  type NodeStackContextOps[K, V]  = TreeStack.ContextOps[K, V, ImmutableTreap.Node]

  type TreapStackOps[K, V, C] = TreeStackOps[K, V, ImmutableTreap, C]

  type NodeStackOps[K, V, C] = TreeStackOps[K, V, ImmutableTreap.Node, C]

  // Tree visit stack
  type TreapVisitStack[K, V] = TreeVisitStack.Stack[K, V, ImmutableTreap]

  type NodeVisitStack[K, V] = TreeVisitStack.Stack[K, V, ImmutableTreap.Node]

  type TreapVisitContext[K, V] = TreeVisitStack.Context[K, V, ImmutableTreap]

  type NodeVisitContext[K, V] = TreeVisitStack.Context[K, V, ImmutableTreap.Node]

  type TreapVisitContextOps[K, V, C <: TreapVisitContext[K, V]]  = TreeVisitStack.ContextOps[K, V, ImmutableTreap, C]

  type NodeVisitContextOps[K, V, C <: NodeVisitContext[K, V]]  = TreeVisitStack.ContextOps[K, V, ImmutableTreap.Node, C]

  // Eval function
  type TreapEvalFunc[K, V, C]  = Eval.BinaryFunc[K, V, ImmutableTreap, C]

  type NodeEvalFunc[K, V, C]  = Eval.BinaryFunc[K, V, ImmutableTreap.Node, C]
}
