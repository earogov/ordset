package ordset.core.segmentSeq.set

import ordset.core.Bound
import ordset.core.segmentSeq.AbstractTreapSegmentSeq
import ordset.core.segmentSeq.AbstractTreapSegmentSeq.TreapSegmentBase
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import ordset.tree.core.eval.TreeVisitStack
import ordset.tree.core.fold.ContextExtract
import ordset.tree.treap.immutable.{ImmutableTreap, NodeVisitContext}
import ordset.tree.treap.immutable.traverse.NodeAside
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap

import java.util.NoSuchElementException
import scala.util.control.NonFatal
import ordset.tree.treap.immutable.traverse.NodeAside

class NonuniformTreapOrderedSet[E, D[X] <: Domain[X]] protected (
  final override val root: ImmutableTreap.Node[Bound.Upper[E], Boolean],
  final override val lastValue: Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractTreapSegmentSeq[E, D, Boolean]
  with OrderedSetCommons[E, D, TreapSegmentBase[E, D, Boolean]]{

  // Set transformation ------------------------------------------------------- //
  override def inverse(implicit ev: Boolean =:= Boolean): OrderedSet[E, D] = 
   // TODO: Temporary solution, see Issue 10.
   defaultInverse

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def getValueForNode(node: ImmutableTreap.Node[Bound.Upper[E], Boolean]): Boolean =
    node.value

  @inline
  protected final override def consUniform(value: Boolean): UniformOrderedSet[E, D] = 
    UniformOrderedSet.default(value)

  @inline
  protected final override def consFromNode(
    node: ImmutableTreap.Node[Bound.Upper[E], Boolean],
    value: Boolean
  ): NonuniformTreapOrderedSet[E, D] =
    NonuniformTreapOrderedSet.uncheckedOptimized(node, value)
}

object NonuniformTreapOrderedSet {

  /**
   * Creates nonuniform ordered set from node of treap containing bounds and values.
   *
   * Validation of treap invariants (keys and priorities order) is not applied.
   * 
   * Method receives last value as an input argument, to avoid its retrieving from tree.
   *
   * @param root node of treap containing upper bounds and values.
   * @param lastValue value of last segment.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  def uncheckedOptimized[E, D[X] <: Domain[X]](
    root: ImmutableTreap.Node[Bound.Upper[E], Boolean],
    lastValue: Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): NonuniformTreapOrderedSet[E, D] =
    new NonuniformTreapOrderedSet(root, lastValue)

  /**
   * Creates nonuniform ordered set from node of treap containing bounds and values.
   *
   * Validation of treap invariants (keys and priorities order) is not applied.
   *
   * @param root node of treap containing upper bounds and values.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  def unchecked[E, D[X] <: Domain[X]](
    root: ImmutableTreap.Node[Bound.Upper[E], Boolean]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): NonuniformTreapOrderedSet[E, D] = {
    // Retrieve last value from tree. Search node with maximal key.
    val contextExtract =
      ContextExtract.foldAfter[Bound.Upper[E], Boolean, ImmutableTreap.Node, NodeVisitContext[Bound.Upper[E], Boolean]](
        root,
        TreeVisitStack.contextOps.getEmptyContext
      )(
        NodeAside.maxKeyFunc(TreeVisitStack.function)
      )
    val stack = contextExtract.context.stack
    // Type of `root` guarantees that it's non-empty tree => stack after maximal key search must be non-empty.
    if (stack.isEmpty) throw new AssertionError(s"Node with maximal key is not found in tree $root")
    // Last node in tree corresponds to penultimate segment => we need to invert its value to get value of last segment.
    val lastValue = !stack.last.tree.value
    NonuniformTreapOrderedSet.uncheckedOptimized(root, lastValue)
  }
}