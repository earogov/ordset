package ordset.core.map

import ordset.core.AbstractTreapSegmentSeq.TreapSegmentBase
import ordset.core._
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.random.RngManager
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap
import ordset.util.ValueHolder

import java.util.NoSuchElementException
import scala.util.control.NonFatal

class NonuniformTreapOrderedMap[E, D <: Domain[E], V] protected (
  final override val root: ImmutableTreap.Node[Bound.Upper[E], V],
  final override val lastValue: V
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val valueOps: ValueOps[V],
  final override val rngManager: RngManager
) extends AbstractTreapSegmentSeq[E, D, V]
  with OrderedMapCommons[E, D, V, TreapSegmentBase[E, D, V]] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def getValueForNode(node: ImmutableTreap.Node[Bound.Upper[E], V]): V = node.value

  @inline
  protected final override def consUniform(value: V): UniformOrderedMap[E, D, V] = UniformOrderedMap.default(value)

  @inline
  protected final override def consFromNode(
    node: ImmutableTreap.Node[Bound.Upper[E], V],
    value: V
  ): NonuniformTreapOrderedMap[E, D, V] =
    NonuniformTreapOrderedMap.unchecked(node, value)
}

object NonuniformTreapOrderedMap {

  /**
   * Creates ordered map from node of treap containing bounds and values.
   *
   * Validation of treap invariants (keys and priorities order) is not applied.
   *
   * @param root node of treap containing upper bounds and values.
   * @param lastValue value of last segment.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc. 
   * @param rngManager generator of random sequences.
   */
  def unchecked[E, D <: Domain[E], V](
    root: ImmutableTreap.Node[Bound.Upper[E], V],
    lastValue: V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): NonuniformTreapOrderedMap[E, D, V] =
    new NonuniformTreapOrderedMap(root, lastValue)
}