package ordset.tree.treap.immutable.transform

import ordset.Order
import ordset.tree.treap.Treap
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.mutable.{MutableTreap, NodeStack => MutableNodeStack}

import scala.annotation.tailrec

/**
 * =Preconditions=
 *
 * Keys are unique and follow in descending order.
 *
 * =Requirements=
 *
 * Build immutable treap in O(n).
 *
 * =Links=
 *
 * See [[BuildAsc]] for details.
 */
object BuildDesc {

  /**
   * Creates new node with `newKey`, `newValue` and `newPriority` and adds it to `buffer`.
   *
   * Warning!
   *
   * Although new buffer instance is returned, state of input `buffer` is modified:
   * children of mutable nodes in buffer are changed during method execution.
   * Commonly after method invocation one should drop old `buffer` and proceed
   * with new one.
   */
  @tailrec
  def addToBuffer[K, KK >: K, V](
    buffer: MutableNodeStack[K, V],
    newKey: K,
    newPriority: Int,
    newValue: V
  )(
    implicit keyOrder: Order[KK]
  ): MutableNodeStack[K, V] = {
    val newNode = new MutableTreap.Node[K, V](newKey, newValue, newPriority)
    buffer match {
      case head :: tail =>
        val cmp = Treap.nodePriorityCompare[KK, V](newPriority, newKey, head.priority, head.key)(keyOrder)
        if (cmp <= 0) {
          newNode.setRightNode(head.getLeftOrNull)
          head.setLeftNode(newNode)
          buffer.prepended(newNode)
        } else {
          // Precondition of method `toImmutableNode` is satisfied due to Lemma 1.
          val immutableHead = BuildUtils.toImmutableNode(head)
          tail match {
            case second :: _ =>
              second.setLeftNode(immutableHead)
              addToBuffer[K, KK, V](tail, newKey, newPriority, newValue)(keyOrder)
            case _ =>
              newNode.setRightNode(immutableHead)
              tail.prepended(newNode)
          }
        }
      case _ =>
        buffer.prepended(newNode)
    }
  }

  /**
   * Converts left front of `tree` to buffer.
   */
  def leftFrontToBuffer[K, V](tree: ImmutableTreap[K, V]): MutableNodeStack[K, V] = {
    @tailrec
    def loop(buffer: MutableNodeStack[K, V], node: ImmutableTreap.Node[K, V]): MutableNodeStack[K, V] = {
      val newNode = new MutableTreap.Node[K, V](node.key, node.value, node.priority)
      newNode.setRightNode(node.getRightOrNull)
      buffer match {
        case head :: _ => head.setLeftNode(newNode)
        case _ => // nothing to do
      }
      val newBuffer = buffer.prepended(newNode)
      node match {
        case node: ImmutableTreap.NodeWithLeft[K, V] => loop(newBuffer, node.left)
        case _ => newBuffer
      }
    }
    tree match {
      case node: ImmutableTreap.Node[K, V] => loop(Nil, node)
      case _ => Nil
    }
  }

  /**
   * Builds new treap from `buffer`.
   */
  @tailrec
  def finalizeBuffer[K, V](
    buffer: MutableNodeStack[K, V],
  ): ImmutableTreap[K, V] =
    buffer match {
      case head :: tail =>
        // Precondition of method `toImmutableNode` is satisfied due to Lemma 1.
        val immutableHead = BuildUtils.toImmutableNode(head)
        tail match {
          case second :: _ =>
            second.setLeftNode(immutableHead)
            finalizeBuffer(tail)
          case _ =>
            immutableHead
        }
      case _ =>
        ImmutableTreap.Empty
    }
}
