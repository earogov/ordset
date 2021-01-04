package ordset.tree.treap.immutable.fold

import ordset.tree.treap.immutable.ImmutableTreap

trait SplitOutput[K, V] {

  def leftTree: ImmutableTreap[K, V]

  def rightTree: ImmutableTreap[K, V]

  def withLeftTree(tree: ImmutableTreap[K, V]): SplitOutput[K, V]

  def withRightTree(tree: ImmutableTreap[K, V]): SplitOutput[K, V]

  def withLeftParent(parent: ImmutableTreap.Node[K, V]): SplitOutput[K, V] = withLeftTree(parent.withRightTree(leftTree))

  def withRightParent(parent: ImmutableTreap.Node[K, V]): SplitOutput[K, V] = withRightTree(parent.withLeftTree(rightTree))

  override def toString: String = s"SplitOutput(leftTree: $leftTree, rightTree: $rightTree)"
}

object SplitOutput {

  object Immutable {

    case class Output[K, V](
      override val leftTree: ImmutableTreap[K, V],
      override val rightTree: ImmutableTreap[K, V]
    ) extends SplitOutput[K, V] {

      override def withLeftTree(tree: ImmutableTreap[K, V]): Output[K, V] = Output(tree, rightTree)

      override def withRightTree(tree: ImmutableTreap[K, V]): Output[K, V] = Output(leftTree, tree)
    }

    object Output {

      def initial[K, V]: SplitOutput[K, V] = Initial.asInstanceOf[SplitOutput[K, V]]

      private lazy val Initial: Output[Any, Any] = Output(ImmutableTreap.Empty, ImmutableTreap.Empty)
    }
  }

  object Mutable {

    class Output[K, V](
      private var leftTreeVar: ImmutableTreap[K, V],
      private var rightTreeVar: ImmutableTreap[K, V]
    ) extends SplitOutput[K, V] {

      override def leftTree: ImmutableTreap[K, V] = leftTreeVar

      override def rightTree: ImmutableTreap[K, V] = rightTreeVar

      override def withLeftTree(tree: ImmutableTreap[K, V]): Output[K, V] = {
        this.leftTreeVar = tree
        this
      }

      override def withRightTree(tree: ImmutableTreap[K, V]): Output[K, V] = {
        this.rightTreeVar = tree
        this
      }
    }

    object Output {

      def initial[K, V]: SplitOutput[K, V] = new Output[K, V](ImmutableTreap.Empty, ImmutableTreap.Empty)
    }
  }
}
