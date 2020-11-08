package ordset.treap.eval

import ordset.domain.Domain
import ordset.treap.Treap

/**
 * Common operations for contexts with node stack.
 */
trait NodeStackOps[E, D <: Domain[E], W, C] {

  /** Returns `true` if node stack is empty. */
  def isEmptyStack(context: C): Boolean

  /**
   * Returns context with empty stack.
   */
  def getEmptyContext: C

  /**
   * Returns head node of the stack if stack is nonempty or `null` otherwise.
   */
  def getHeadNodeOrNull(context: C): Treap.Node[E, D, W]

  /**
   * Returns head node if stack is nonempty or [[Treap.Empty]] otherwise.
   */
  def getHeadTree(context: C): Treap[E, D, W]

  /**
   * Returns context of parent node if stack is nonempty or null otherwise.
   */
  def getParentContextOrNull(context: C): C

  /**
   * Returns context of parent node if stack is nonempty or context with empty stack otherwise.
   */
  def getParentContext(context: C): C
}
