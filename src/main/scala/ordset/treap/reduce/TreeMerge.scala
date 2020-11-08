package ordset.treap.reduce

import ordset.Order
import ordset.domain.Domain
import ordset.treap.Treap
import ordset.treap.eval.NodeStack
import ordset.treap.traverse.KeySearch

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
 * val leftExtract = ContextExtract.reduceAfter(
 *    leftTree,
 *    NodeStack.contextOps[E, D, W].getEmptyContext
 * )(
 *    KeySearch.maxKey(NodeStack.of(leftTree))
 * )
 * }}}
 *
 *    Add the last node H to the stack.
 *
 * {{{
 * val leftStack = NodeStack.contextOps.getChildTreeContext(leftExtract.context, leftExtract.tree)
 * }}}
 *
 * 2. Move down from the right tree root to the min key (the most left) and build stack of visited nodes.
 *    We start from node A and stop at node E. Stack will contain one node E.
 *
 * {{{
 * val rightExtract = ContextExtract.reduceAfter(
 *    rightTree,
 *    NodeStack.contextOps[E, D, W].getEmptyContexts
 * )(
 *    KeySearch.minKey(NodeStack.of(rightTree))
 * )
 * }}}
 *
 *    Add the last node E to the stack.
 *
 * {{{
 * val rightStack = NodeStack.contextOps.getChildTreeContext(rightExtract.context, rightExtract.tree)
 * }}}
 *
 * 3. Apply [[TreeMerge.mergeFunc]] to the received stacks to build merged tree.
 *
 * {{{
 * mergeFunc(leftStack, rightStack, Treap.Empty())
 * }}}
 */
object TreeMerge {

  type Stack[E, D <: Domain[E], W] = NodeStack.Stack[E, D, W]

  @tailrec
  def mergeFunc[E, D <: Domain[E], W](
    leftStack: Stack[E, D, W],
    rightStack: Stack[E, D, W],
    mergedTree: Treap[E, D, W]
  )(
    implicit priorityOrder: Order[Treap.Node[E, D, W]]
  ): Treap[E, D, W] = {
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
  def mergeLeftFunc[E, D <: Domain[E], W](
    leftStack: Stack[E, D, W],
    mergedTree: Treap[E, D, W]
  ): Treap[E, D, W] =
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
  def mergeRightFunc[E, D <: Domain[E], W](
    rightStack: Stack[E, D, W],
    mergedTree: Treap[E, D, W]
  ): Treap[E, D, W] =
    rightStack match {
      //             rightHead
      //              /    \
      //       mergedTree  ...
      //         /   \
      //       ...   ...
      case rightHead :: rightTail => mergeRightFunc(rightTail, rightHead.withLeftTree(mergedTree))
      case _ => mergedTree
  }

  def reduce[E, D <: Domain[E], W](
    leftTree: Treap[E, D, W],
    rightTree: Treap[E, D, W]
  )(
    implicit priorityOrder: Order[Treap.Node[E, D, W]]
  ): Treap[E, D, W] = {

    val leftExtract = ContextExtract.reduceAfter(
      leftTree,
      NodeStack.contextOps[E, D, W].getEmptyContext
    )(
      KeySearch.maxKey(NodeStack.of(leftTree))
    )
    val leftStack = NodeStack.contextOps.getChildTreeContext(leftExtract.context, leftExtract.tree)

    val rightExtract = ContextExtract.reduceAfter(
      rightTree,
      NodeStack.contextOps[E, D, W].getEmptyContext
    )(
      KeySearch.minKey(NodeStack.of(rightTree))
    )
    val rightStack = NodeStack.contextOps.getChildTreeContext(rightExtract.context, rightExtract.tree)

    mergeFunc(leftStack, rightStack, Treap.Empty())
  }
}
