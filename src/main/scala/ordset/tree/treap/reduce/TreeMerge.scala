package ordset.tree.treap.reduce

import ordset.Order
import ordset.tree.treap.Treap
import ordset.tree.core.eval.TreeStack
import ordset.tree.core.reduce.ContextExtract
import ordset.tree.treap.eval.NodeStack
import ordset.tree.treap.traverse.NodeSearch

import scala.annotation.tailrec

/**
 * [[TreeMerge.mergeFunc]] implements merge operation for two trees.
 *
 * Precondition: max key of left tree `<` min key of right tree
 *
 * {{{
 *               left tree           right tree
 *
 *    9  -                               A
 *    8  -                             /   ↘
 *    7  -             B              /       ↘
 *    6  -       ↙      \            /          C
 *    5  -    ↙          \          E             ↘
 *    4  - D              \                         ↘
 *    3  -    ↘            \                         G
 *    2  -       F          \
 *    1  -                   H
 *         |-----|-----|-----|-----|-----|-----|-----|
 *         1     2     3     4     5     6     7     8
 *
 *                                   mergedTree
 *
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
 *         1     2     3     4     5     6     7     8
 *
 * }}}
 * =Usage=
 *
 * 1. Move down from the left tree root to the max key (the most right) and build stack of visited nodes.
 *    In example above we start from node B and stop at node H. Stack will contain one node H.
 *
 * {{{
 *     val leftExtract = ContextExtract.reduceAfter(
 *       leftTree,
 *       TreeStack.contextOps[K, V, Treap.Node].getEmptyContext
 *     )(
 *       NodeSearch.maxKey(TreeStack.function())
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
 *     val rightExtract = ContextExtract.reduceAfter(
 *       rightTree,
 *       TreeStack.contextOps[K, V, Treap.Node].getEmptyContext
 *     )(
 *       NodeSearch.minKey(TreeStack.function())
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

  @tailrec
  def mergeFunc[K, V](
    leftStack: NodeStack[K, V],
    rightStack: NodeStack[K, V],
    mergedTree: Treap[K, V]
  )(
    implicit priorityOrder: Order[Treap.Node[K, V]]
  ): Treap[K, V] = {
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
          mergeFunc(leftTail, rightTail, rightHead.withLeftNode(leftHead.withRightTree(mergedTree)))
        } else {
          //        leftHead
          //         /    \
          //       ...  rightHead
          //              /    \
          //       mergedTree  ...
          //         /   \
          //       ...   ...
          mergeFunc(leftTail, rightTail, leftHead.withRightNode(rightHead.withLeftTree(mergedTree)))
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
    mergedTree: Treap[K, V]
  ): Treap[K, V] =
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
    mergedTree: Treap[K, V]
  ): Treap[K, V] =
    rightStack match {
      //             rightHead
      //              /    \
      //       mergedTree  ...
      //         /   \
      //       ...   ...
      case rightHead :: rightTail => mergeRightFunc(rightTail, rightHead.withLeftTree(mergedTree))
      case _ => mergedTree
  }

  def reduceNode[K, V](
    leftNode: Treap.Node[K, V],
    rightNode: Treap.Node[K, V]
  )(
    implicit priorityOrder: Order[Treap.Node[K, V]]
  ): Treap[K, V] = {

    val leftExtract = ContextExtract.reduceAfter(
      leftNode,
      TreeStack.contextOps[K, V, Treap.Node].getEmptyContext
    )(
      NodeSearch.maxKey(TreeStack.function())
    )
    val leftStack = TreeStack.contextOps.addToStack(leftExtract.context, leftExtract.tree)

    val rightExtract = ContextExtract.reduceAfter(
      rightNode,
      TreeStack.contextOps[K, V, Treap.Node].getEmptyContext
    )(
      NodeSearch.minKey(TreeStack.function())
    )
    val rightStack = TreeStack.contextOps.addToStack(rightExtract.context, rightExtract.tree)

    mergeFunc(leftStack, rightStack, Treap.Empty())
  }

  def reduceTreap[K, V](
    leftTree: Treap[K, V],
    rightTree: Treap[K, V]
  )(
    implicit priorityOrder: Order[Treap.Node[K, V]]
  ): Treap[K, V] =
    leftTree match {
      case leftNode: Treap.Node[K, V] =>
        rightTree match {
          case rightNode: Treap.Node[K, V] => reduceNode(leftNode, rightNode)(priorityOrder)
          case _ => leftNode
      }
      case _ => rightTree
    }
}
