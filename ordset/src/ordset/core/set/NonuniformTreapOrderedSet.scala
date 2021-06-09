package ordset.core.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.core._
import ordset.random.RngManager
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap

import java.util.NoSuchElementException
import scala.util.control.NonFatal

class NonuniformTreapOrderedSet[E, D <: Domain[E]] protected (
  final override val root: ImmutableTreap.Node[Bound.Upper[E], Boolean],
  final override val lastValue: Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractTreapSegmentSeq[E, D, Boolean]
  with OrderedSetCommons[E, D]{

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: Boolean): UniformOrderedSet[E, D] =
    UniformOrderedSet.apply(value, TreapOrderedSet.getFactory)

  @inline
  protected final override def consFromNode(
    node: ImmutableTreap.Node[Bound.Upper[E], Boolean],
    value: Boolean
  ): NonuniformTreapOrderedSet[E, D] =
    NonuniformTreapOrderedSet.unchecked(node, value)

  @inline
  protected final override def isValueIncluded(value: Boolean): Boolean = value
}

object NonuniformTreapOrderedSet {

  /**
   * Creates nonuniform ordered set from node of treap containing bounds and values.
   *
   * Validation of treap invariants (keys and priorities order) is not applied.
   *
   * @param root node of treap containing upper bounds and values.
   * @param lastValue value of last segment.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  def unchecked[E, D <: Domain[E]](
    root: ImmutableTreap.Node[Bound.Upper[E], Boolean],
    lastValue: Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): NonuniformTreapOrderedSet[E, D] =
    new NonuniformTreapOrderedSet(root, lastValue)
}