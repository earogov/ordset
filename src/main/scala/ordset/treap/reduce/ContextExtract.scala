package ordset.treap.reduce

import ordset.domain.Domain
import ordset.treap.{Reduce, Traverse, Treap}

object ContextExtract {

  case class Output[E, D <: Domain[E], W, C](tree: Treap[E, D, W], context: C)

  object Output {

    def initial[E, D <: Domain[E], W, C](context: C): Output[E, D, W, C] = Output(Treap.Empty(), context)

    def initialUnsafe[E, D <: Domain[E], W, C]: Output[E, D, W, C] = InitialUnsafe.asInstanceOf[Output[E, D, W, C]]

    private lazy val InitialUnsafe: Output[Any, Domain[Any], Any, Any] = Output(Treap.Empty(), null)
  }

  def function[E, D <: Domain[E], W, C]: Reduce.Func[E, D, W, C, Output[E, D, W, C]] =
    ExtractFunc.asInstanceOf[Reduce.Func[E, D, W, C, Output[E, D, W, C]]]

  def reduceBefore[E, D <: Domain[E], W, C](
    treap: Treap[E, D, W],
    initContext: C
  )(
    traverseFunc: Traverse.GenericFunc[E, D, W, C]
  ): Output[E, D, W, C] =
    Reduce.before(treap, initContext, Output.initialUnsafe[E, D, W, C])(traverseFunc, function[E, D, W, C])

  def reduceAfter[E, D <: Domain[E], W, C](
    treap: Treap[E, D, W],
    initContext: C,
  )(
    traverseFunc: Traverse.GenericFunc[E, D, W, C]
  ): Output[E, D, W, C] =
    Reduce.after(treap, initContext, Output.initialUnsafe[E, D, W, C])(traverseFunc, function[E, D, W, C])

  private lazy val ExtractFunc: Reduce.Func[Any, Domain[Any], Any, Any, Output[Any, Domain[Any], Any, Any]] =
    (tree, context, _) => Output(tree, context)
}
