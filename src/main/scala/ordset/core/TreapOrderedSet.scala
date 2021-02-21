package ordset.core

import ordset.core.domain.{Domain, DomainOps, OrderValidationFunc}
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap
import ordset.util.IterableUtil

import java.util.NoSuchElementException

class TreapOrderedSet[E, D <: Domain[E]] protected(
  final override val root: ImmutableTreap.Node[Bound.Upper[E], Boolean],
  final override val lastValue: Boolean
)(
  implicit final override val domainOps: DomainOps[E, D]
) extends AbstractTreapSegmentSeq[E, D, Boolean] {

  // Protected section -------------------------------------------------------- //
  protected def consUniform(value: Boolean): SegmentSeq[E, D, Boolean] =
    UniformOrderedSet.apply(value)

  protected def consFromNode(
    node: ImmutableTreap.Node[Bound.Upper[E], Boolean],
    value: Boolean
  ): SegmentSeq[E, D, Boolean] =
    TreapOrderedSet.unchecked(node, value)

  @inline
  protected final override def isIncludedInSet(value: Boolean): Boolean = value
}

object TreapOrderedSet {

  /**
   * Creates ordered set from treap node (see [[AbstractSegmentSeq]]).
   *
   * Validation of treap invariants (key and priorities order) is not applied.
   */
  def unchecked[E, D <: Domain[E]](
    root: ImmutableTreap.Node[Bound.Upper[E], Boolean],
    lastValue: Boolean
  )(
    implicit domainOps: DomainOps[E, D]
  ): OrderedSet[E, D] = new TreapOrderedSet(root, lastValue)

  /**
   * Creates ordered set from collection of upper bounds.
   *
   * Preconditions:
   *
   * 1. Size of `priorities` collection must be greater or equal to size of `bounds` collection:
   *
   * priorities.size `>=` bounds.size
   *
   * 2. `bounds` collection is ordered according to `validationFunc`:
   *
   * validationFunc(bounds^i-1^, bounds^i^) == true for each i in [1, bounds.size]
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def fromIterableUnsafe[E, D <: Domain[E]](
    bounds: IterableOnce[Bound.Upper[E]],
    priorities: IterableOnce[Int],
    complementary: Boolean,
    domainOps: DomainOps[E, D]
  )(
    validationFunc: OrderValidationFunc[Bound.Upper[E]] = domainOps.boundOrd.strictValidationFunc
  ): OrderedSet[E, D] = {
    val boundOrd = domainOps.domain.boundOrd
    val intOrd = domainOps.domain.intOrd
    val priorityIterator = priorities.iterator
    try {
      var value = complementary
      val buffer =
        OrderValidationFunc.foldIterableAfter[Bound.Upper[E], List[MutableTreap.Node[Bound.Upper[E], Boolean]]](
          bounds,
          validationFunc,
          List.empty[MutableTreap.Node[Bound.Upper[E], Boolean]],
          (buf, bnd) => {
            val priority =IterableUtil.nextOrThrowMsg(
              priorityIterator,
              "Size of priorities collection must be greater or equal to size of bounds collection."
            )
            val buffer = BuildAsc.appendToBuffer[Bound.Upper[E], Bound[E], Boolean](
              buf, bnd, priority, value
            )(
              intOrd, boundOrd
            )
            value = !value
            buffer
          }
        )
      val root = BuildAsc.finalizeBuffer(buffer)
      root match {
        case r: ImmutableTreap.Node[Bound.Upper[E], Boolean] => new TreapOrderedSet(r, value)(domainOps)
        case _ => UniformOrderedSet(value)(domainOps)
      }
    } catch {
      case e @ (_: NoSuchElementException | _: IllegalArgumentException) => throw SegmentSeqException(e)
    }
  }

  /**
   * Returns ordered set factory.
   */
  def getFactory[E, D <: Domain[E]](
    priorities: IterableOnce[Int],
    domainOps: DomainOps[E, D]
  )(
    validationFunc: OrderValidationFunc[Bound.Upper[E]] = domainOps.boundOrd.strictValidationFunc
  ): OrderedSetFactory[E, D] =
    (bounds, complementary) => fromIterableUnsafe[E, D](bounds, priorities, complementary, domainOps)(validationFunc)
}