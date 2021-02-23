package ordset.tree.core.eval

/**
 * Common operations for contexts with node stack.
 */
trait TreeStackOps[K, V, Tree[KK, VV], C] {

  /**
   * Returns `true` if node stack is empty.
   */
  def isEmptyStack(context: C): Boolean

  /**
   * Returns context with empty stack.
   */
  def getEmptyContext: C

  /**
   * Returns tree from stack head or `default` if stack is empty.
   */
  def getHeadTreeOrDefault(context: C, default: Tree[K, V]): Tree[K, V]

  /**
   * Returns context for stack tail or `default` if stack is empty.
   */
  def getTailContextOrDefault(context: C, default: C): C

  /**
   * Returns context for stack tail. If stack is empty returns context with empty stack.
   * In that case the result is equivalent to call of [[getEmptyContext]].
   */
  def getTailContextOrEmpty(context: C): C = getTailContextOrDefault(context, getEmptyContext)
}
