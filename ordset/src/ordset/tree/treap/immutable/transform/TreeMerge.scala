package ordset.tree.treap.immutable.transform

import ordset.Order
import ordset.tree.core.eval.TreeStack
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.Treap
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.NodeStack
import ordset.tree.treap.immutable.traverse.NodeAside

import scala.annotation.tailrec

/**
 * Merge operation assembles two treaps into one.
 *
 * Precondition: max key of left tree `<` min key of right tree
 *
 * {{{
 *  priority
 *                                   right tree
 *    9  -                               A
 *    8  -         left tree           /   ↘
 *    7  -             B              /       ↘
 *    6  -       ↙      \            /          C
 *    5  -    ↙          \          E             ↘
 *    4  - D              \                         ↘
 *    3  -    ↘            \                         G
 *    2  -       F          \
 *    1  -                   H
 *         |-----|-----|-----|-----|-----|-----|-----|
 *         1     2     3     4     5     6     7     8  key
 *
 *
 *  priority
 *                                  merged tree
 *    9  -                               A
 *    8  -                      ↙          ↘
 *    7  -             B                     ↘
 *    6  -       ↙           ↘                 C
 *    5  -    ↙                    E             ↘
 *    4  - D                      ↙                ↘
 *    3  -    ↘                 ↙                    G
 *    2  -       F            ↙
 *    1  -                   H
 *         |-----|-----|-----|-----|-----|-----|-----|
 *         1     2     3     4     5     6     7     8  key
 *
 * }}}
 * =Usage=
 *
 * 1. Move down from the left tree root to the max key (the most right) and build stack of visited nodes.
 *    In example above we start from node B and stop at node H. Stack will contain one node H.
 *
 * {{{
 *     val leftExtract = ContextExtract.foldAfter(
 *       leftTree,
 *       TreeStack.contextOps[K, V, Treap.Node].getEmptyContext
 *     )(
 *       NodeSearch.maxKey(TreeStack.function)
 *     )
 * }}}
 *
 *    Add the last node H to the stack.
 *
 * {{{
 *     val leftStack = TreeStack.contextOps.addToStack(leftExtract.context, leftExtract.tree)
 * }}}
 *
 * 2. Move down from the right tree root to the min key (the most left) and build stack of visited nodes.
 *    We start from node A and stop at node E. Stack will contain one node E.
 *
 * {{{
 *     val rightExtract = ContextExtract.foldAfter(
 *       rightTree,
 *       TreeStack.contextOps[K, V, Treap.Node].getEmptyContext
 *     )(
 *       NodeSearch.minKey(TreeStack.function)
 *     )
 * }}}
 *
 *    Add the last node E to the stack.
 *
 * {{{
 *     val rightStack = TreeStack.contextOps.addToStack(rightExtract.context, rightExtract.tree)
 * }}}
 *
 * 3. Apply [[TreeMerge.mergeFunc]] to the received stacks to build merged tree.
 *
 * {{{
 *     mergeFunc(leftStack, rightStack, Treap.Empty())
 * }}}
 */
object TreeMerge {

  /**
   * Returns function that implements one step of merge operation.
   */
  @tailrec
  def mergeFunc[K, KK >: K, V](
    leftStack: NodeStack[K, V],
    rightStack: NodeStack[K, V],
    mergedTree: ImmutableTreap[K, V]
  )(
    implicit priorityOrder: Order[Treap.Node[KK, V]]
  ): ImmutableTreap[K, V] = {
    (leftStack, rightStack) match {
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (priorityOrder.compare(leftHead, rightHead) <= 0) {
          //       rightHead
          //        /    \
          //  leftHead   ...
          //   /    \
          // ...   mergedTree
          //         /   \
          //       ...   ...
          mergeFunc[K, KK, V](leftTail, rightTail, rightHead.withLeftNode(leftHead.withRightTree(mergedTree)))
        } else {
          //        leftHead
          //         /    \
          //       ...  rightHead
          //              /    \
          //       mergedTree  ...
          //         /   \
          //       ...   ...
          mergeFunc[K, KK, V](leftTail, rightTail, leftHead.withRightNode(rightHead.withLeftTree(mergedTree)))
        }
      case (leftHead :: leftTail, _) =>
        //  leftHead   ...
        //   /    \
        // ...   mergedTree
        //         /   \
        //       ...   ...
        mergeLeftFunc(leftTail, leftHead.withRightTree(mergedTree))
      case (_, rightHead :: rightTail) =>
        //             rightHead
        //              /    \
        //       mergedTree  ...
        //         /   \
        //       ...   ...
        mergeRightFunc(rightTail, rightHead.withLeftTree(mergedTree))
      case _ =>
        mergedTree
    }
  }

  /**
   * Same as
   * {{{ mergeFunc(leftStack, Nil, mergedTree) }}}
   */
  @tailrec
  def mergeLeftFunc[K, V](
    leftStack: NodeStack[K, V],
    mergedTree: ImmutableTreap[K, V]
  ): ImmutableTreap[K, V] =
    leftStack match {
      //  leftHead
      //   /    \
      // ...   mergedTree
      //         /   \
      //       ...   ...
      case leftHead :: leftTail => mergeLeftFunc(leftTail, leftHead.withRightTree(mergedTree))
      case _ => mergedTree
    }

  /**
   * Same as
   * {{{ mergeFunc(Nil, rightStack, mergedTree) }}}
   */
  @tailrec
  def mergeRightFunc[K, V](
    rightStack: NodeStack[K, V],
    mergedTree: ImmutableTreap[K, V]
  ): ImmutableTreap[K, V] =
    rightStack match {
      //             rightHead
      //              /    \
      //       mergedTree  ...
      //         /   \
      //       ...   ...
      case rightHead :: rightTail => mergeRightFunc(rightTail, rightHead.withLeftTree(mergedTree))
      case _ => mergedTree
  }

  /**
   * Applies [[mergeFunc]] function to `leftNode` and `rightNode` (non empty) treaps.
   */
  def foldNode[K, KK >: K, V](
    leftNode: ImmutableTreap.Node[K, V],
    rightNode: ImmutableTreap.Node[K, V]
  )(
    implicit priorityOrder: Order[Treap.Node[KK, V]]
  ): ImmutableTreap[K, V] = {

    val leftExtract = ContextExtract.foldAfter(
      leftNode,
      TreeStack.contextOps[K, V, ImmutableTreap.Node].getEmptyContext
    )(
      NodeAside.maxKeyFunc(TreeStack.function)
    )
    val leftStack = TreeStack.contextOps.addToStack(leftExtract.context, leftExtract.tree)

    val rightExtract = ContextExtract.foldAfter(
      rightNode,
      TreeStack.contextOps[K, V, ImmutableTreap.Node].getEmptyContext
    )(
      NodeAside.minKeyFunc(TreeStack.function)
    )
    val rightStack = TreeStack.contextOps.addToStack(rightExtract.context, rightExtract.tree)

    mergeFunc[K, KK, V](leftStack, rightStack, ImmutableTreap.Empty)
  }

  /**
   * Applies [[mergeFunc]] function to `leftTree` and `rightTree` (possibly empty) treaps.
   */
  def foldTreap[K, KK >: K, V](
    leftTree: ImmutableTreap[K, V],
    rightTree: ImmutableTreap[K, V]
  )(
    implicit priorityOrder: Order[Treap.Node[KK, V]]
  ): ImmutableTreap[K, V] =
    leftTree match {
      case leftNode: ImmutableTreap.Node[K, V] =>
        rightTree match {
          case rightNode: ImmutableTreap.Node[K, V] =>
            foldNode[K, KK, V](leftNode, rightNode)(priorityOrder)
          case _ =>
            leftNode
      }
      case _ =>
        rightTree
    }
}