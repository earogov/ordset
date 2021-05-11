package ordset.tree.treap.immutable.transform

import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.mutable.MutableTreap

object BuildUtils {

  /**
   * Converts mutable node to immutable one.
   *
   * Preconditions:
   *
   * 1. All children of input node must be immutable.
   */
  def toImmutableNode[K, V](mutable: MutableTreap.Node[K, V]): ImmutableTreap.Node[K, V] =
    if (mutable.getLeftOrNull == null)
      if (mutable.getRightOrNull == null)
        new ImmutableTreap.Leaf[K, V](
          mutable.key,
          mutable.priority,
          mutable.value
        )
      else
        new ImmutableTreap.NodeWithRightOnly[K, V](
          // Cast is safe by precondition.
          mutable.getRightOrNull.asInstanceOf[ImmutableTreap.Node[K, V]],
          mutable.key,
          mutable.priority,
          mutable.value
        )
    else
      if (mutable.getRightOrNull == null)
        new ImmutableTreap.NodeWithLeftOnly[K, V](
          // Cast is safe by precondition.
          mutable.getLeftOrNull.asInstanceOf[ImmutableTreap.Node[K, V]],
          mutable.key,
          mutable.priority,
          mutable.value
        )
      else
        new ImmutableTreap.NodeWithLeftRight[K, V](
          // Casts are safe by precondition.
          mutable.getLeftOrNull.asInstanceOf[ImmutableTreap.Node[K, V]],
          mutable.getRightOrNull.asInstanceOf[ImmutableTreap.Node[K, V]],
          mutable.key,
          mutable.priority,
          mutable.value
        )
}
