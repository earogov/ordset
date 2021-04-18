package ordset.tree.treap.immutable.transform

import ordset.Order
import ordset.tree.treap.Treap
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.mutable.MutableTreap
import ordset.tree.treap.mutable.{NodeStack => MutableNodeStack}

import scala.annotation.tailrec

/**
 * =Preconditions=
 *
 * Keys are unique and follow in ascending order.
 *
 * =Requirements=
 *
 * Build immutable treap in O(n).
 *
 * =Implementation=
 *
 * Suppose one moves down from the tree root and chooses each time right child. Let's call all visited nodes
 * the <u>right front</u> of tree. On example below right front includes nodes A, B, C.
 * {{{
 *
 *       A  <- right front
 *      / \
 *     D   B  <- right front
 *    / \ / \
 *  ... ...  C  <- right front
 * }}}
 * To create a treap we need some temporary mutable nodes. All of them will be a part of right front.
 * We will keep them in buffer list.
 * Denote mutable nodes with suffix `m` and immutable - with suffix `i`. Assume that at some intermediate step
 * we have tree like shown below and buffer list Bm :: Am :: Nil.
 * {{{
 *
 *        Am
 *       /  \
 *      Di   Bm  <- buffer head
 *     /    /  \
 *   ...   Ei   Ci
 *        / \
 *      ...  Fi
 * }}}
 * We need to append some new node with key `newKey`, value `newValue` and priority `newPriority`.
 *
 * 1. Take buffer head Bm and compare its priority with `newPriority`.
 *
 * 2. If Priority of Bm `<` `newPriority` then:
 *
 * 2.1. Remove Bm from buffer list. The result list will be Am :: Nil.
 *
 * 2.2. Replace Bm with immutable node (with the same key, value and priority), because this node will never
 *      be visited again and changed.
 *
 * 2.3. Take buffer head Am and return to step 1 with it.
 * {{{
 *
 *        Am  <- buffer head
 *       /  \
 *      Di   Bi
 *     /    /  \
 *   ...   Ei   Ci
 *        / \
 *      ...  Fi
 * }}}
 *
 * 3. If priority of Bm `>` `newPriority` then:
 *
 * 3.1. Create new mutable node Nm with key `newKey`, value `newValue` and priority `newPriority`.
 *
 * 3.2. Make Nm the right child of Bm.
 *
 * 3.3. Make Ci (previous right child of Bm) the left child of Nm.
 *
 * 3.4. Prepend Nm to buffer list. The result list will be Nm :: Bm :: Am :: Nil.
 * {{{
 *
 *         Am
 *       /   \
 *      Di    Bm
 *     /    /   \
 *   ...   Ei    Nm  <- buffer head
 *        / \    /
 *      ... Fi  Ci
 * }}}
 *
 * 4. Suppose after step 2.3 priority of Am `<` `newPriority` then:
 *
 * 4.1. Remove Am from buffer list. The result list will be empty (Nil).
 *
 * 4.2. Replace Am with immutable node Ai.
 *
 * 4.3. Create new mutable node Nm with key `newKey`, value `newValue` and priority `newPriority`.
 *
 * 4.4. Make Ai the left child of Nm.
 *
 * 4.5. Prepend Nm to buffer list. The result list will be Nm :: Nil.
 * {{{
 *
 *           Nm
 *          /
 *        Ai
 *       /  \
 *      Di   Bi
 *     /    /  \
 *   ...   Ei   Ci
 *        / \
 *      ...  Fi
 * }}}
 *
 * When all nodes are added to treap it only remains to convert all mutable nodes from buffer to equivalent immutable.
 * Last element of buffer will be the root of constructed immutable treap.
 *
 * Notes
 *
 * 1. We don't consider case of equal priorities thought there is some chance that two random priorities will be
 *    the same for different nodes. In such case we can use nodes keys for comparison (which are different by
 *    precondition). So we can always get strict `less` or `greater` relation for any two nodes.
 *
 * =Lemma 1=
 *
 * Both children of buffer head are immutable.
 *
 * 1. Right child
 *
 * According to steps 2.1 and 2.2 each time we drop current buffer head Bm (and second element Am becomes new head)
 * we replace Bm with immutable node Bi. So right child of new head Am becomes immutable.
 *
 * When we prepend to buffer new node Nm at step 3.4 it has no right child, so buffer head will never have mutable
 * right child.
 *
 * 2. Left child
 *
 * Node Ci becomes left child of new buffer head Nm at step 3.3. Previously it was right child of former buffer head.
 * So according to point 1 of current proof it's immutable.
 *
 * Node Ai also becomes left child of buffer head Nm at step 4.4. At step 4.2 it was made immutable, so buffer head
 * can't have mutable left child.
 *
 * =Performance=
 *
 * During treap construction each new node is visited twice: first time when it is added to buffer list and second -
 * when removed. So the total time is O(n) from the number of added nodes.
 *
 * =Usage=
 *
 * 1a. Add new nodes to empty buffer.
 * {{{
 *
 * val buffer =
 *   BuildAsc.addToBuffer(
 *     Nil, newKey1, newPriority1, newValue1
 *   )(
 *     boundOrd
 *   )
 * val buffer =
 *   BuildAsc.addToBuffer(
 *     buffer, newKey2, newPriority2, newValue2
 *   )(
 *     boundOrd
 *   )
 * }}}
 *
 * 1b. Or convert right front of existing tree into buffer and add new nodes there.
 * {{{
 *
 * val buffer = BuildAsc.rightFrontToBuffer(someTree)
 *
 * val buffer =
 *   BuildAsc.addToBuffer(
 *     buffer, newKey1, newPriority1, newValue1
 *   )(
 *     boundOrd
 *   )
 * }}}
 *
 * 2. Build new treap from buffer.
 * {{{
 *
 * val treap = BuildAsc.finalizeBuffer(buffer)
 * }}}
 */
object BuildAsc {

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
          newNode.setLeftNode(head.getRightOrNull)
          head.setRightNode(newNode)
          buffer.prepended(newNode)
        } else {
          // Precondition of method `toImmutableNode` is satisfied due to Lemma 1.
          val immutableHead = toImmutableNode(head)
          tail match {
            case second :: _ =>
              second.setRightNode(immutableHead)
              addToBuffer[K, KK, V](tail, newKey, newPriority, newValue)(keyOrder)
            case _ =>
              newNode.setLeftNode(immutableHead)
              tail.prepended(newNode)
          }
        }
      case _ =>
        buffer.prepended(newNode)
    }
  }

  /**
   * Converts right front of `tree` to buffer.
   */
  def rightFrontToBuffer[K, V](tree: ImmutableTreap[K, V]): MutableNodeStack[K, V] = {
    @tailrec
    def loop(buffer: MutableNodeStack[K, V], node: ImmutableTreap.Node[K, V]): MutableNodeStack[K, V] = {
      val newNode = new MutableTreap.Node[K, V](node.key, node.value, node.priority)
      newNode.setLeftNode(node.getLeftOrNull)
      buffer match {
        case head :: _ => head.setRightNode(newNode)
        case _ => // nothing to do
      }
      val newBuffer = buffer.prepended(newNode)
      node match {
        case node: ImmutableTreap.NodeWithRight[K, V] => loop(newBuffer, node.right)
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
        val immutableHead = toImmutableNode(head)
        tail match {
          case second :: _ =>
            second.setRightNode(immutableHead)
            finalizeBuffer(tail)
          case _ =>
            immutableHead
        }
      case _ =>
        ImmutableTreap.Empty
    }

  /**
   * Converts mutable node to immutable one.
   *
   * Preconditions:
   *
   * 1. All children of input node must be immutable.
   */
  private def toImmutableNode[K, V](mutable: MutableTreap.Node[K, V]): ImmutableTreap.Node[K, V] =
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
