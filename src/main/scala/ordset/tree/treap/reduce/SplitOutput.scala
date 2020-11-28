package ordset.tree.treap.reduce

import ordset.tree.treap.Treap

trait SplitOutput[K, V] {

  def leftTree: Treap[K, V]

  def rightTree: Treap[K, V]

  def withLeftTree(tree: Treap[K, V]): SplitOutput[K, V]

  def withRightTree(tree: Treap[K, V]): SplitOutput[K, V]

  def withLeftParent(parent: Treap.Node[K, V]): SplitOutput[K, V] = withLeftTree(parent.withRightTree(leftTree))

  def withRightParent(parent: Treap.Node[K, V]): SplitOutput[K, V] = withRightTree(parent.withLeftTree(rightTree))

  override def toString: String = s"SplitOutput(leftTree: $leftTree, rightTree: $rightTree)"
}

object SplitOutput {

  object Immutable {

    case class Output[K, V](
      override val leftTree: Treap[K, V],
      override val rightTree: Treap[K, V]
    ) extends SplitOutput[K, V] {

      override def withLeftTree(tree: Treap[K, V]): Output[K, V] = Output(tree, rightTree)

      override def withRightTree(tree: Treap[K, V]): Output[K, V] = Output(leftTree, tree)
    }

    object Output {

      def initial[K, V]: SplitOutput[K, V] = Initial.asInstanceOf[SplitOutput[K, V]]

      private lazy val Initial: Output[Any, Any] = Output(Treap.Empty(), Treap.Empty())
    }
  }

  object Mutable {

    class Output[K, V](
      private var leftTreeVar: Treap[K, V],
      private var rightTreeVar: Treap[K, V]
    ) extends SplitOutput[K, V] {

      override def leftTree: Treap[K, V] = leftTreeVar

      override def rightTree: Treap[K, V] = rightTreeVar

      override def withLeftTree(tree: Treap[K, V]): Output[K, V] = {
        this.leftTreeVar = tree
        this
      }

      override def withRightTree(tree: Treap[K, V]): Output[K, V] = {
        this.rightTreeVar = tree
        this
      }
    }

    object Output {

      def initial[K, V]: SplitOutput[K, V] = new Output[K, V](Treap.Empty(), Treap.Empty())
    }
  }
}
