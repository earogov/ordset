package ordset.core

import ordset.core.domain.{Domain, DomainOps, OrderValidationFunc}
import ordset.random.UnsafeUniformRng
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap

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
   * 1. `bounds` collection is ordered according to `validationFunc`:
   * {{{
   *   validationFunc(bounds^i-1^, bounds^i^) == true for each i in [1, bounds.size]
   * }}}
   *
   * Method is considered 'unsafe' because it throws exception if preconditions are violated.
   *
   * @param bounds collection of upper bounds.
   * @param random random number generator with uniform distribution. Generates priorities for treap nodes.
   * @param complementary when `true`: first segment is included in set, second - excluded, etc;
   *                      when `false`: first segment is excluded, second - included, etc.
   * @param domainOps domain related functions.
   * @param validationFunc validates ordering of `bounds`.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def fromIterableUnsafe[E, D <: Domain[E]](
    bounds: IterableOnce[Bound.Upper[E]],
    random: UnsafeUniformRng,
    complementary: Boolean,
    domainOps: DomainOps[E, D]
  )(
    validationFunc: OrderValidationFunc[Bound.Upper[E]] = domainOps.boundOrd.strictValidationFunc
  ): OrderedSet[E, D] = {
    val boundOrd = domainOps.domain.boundOrd
    try {
      var value = complementary
      val buffer =
        OrderValidationFunc.foldIterableAfter[Bound.Upper[E], List[MutableTreap.Node[Bound.Upper[E], Boolean]]](
          bounds,
          validationFunc,
          List.empty[MutableTreap.Node[Bound.Upper[E], Boolean]],
          (buf, bnd) => {
            val buffer =
              BuildAsc.appendToBuffer[Bound.Upper[E], Bound[E], Boolean](
                buf, bnd, random.nextInt(), value
              )(
                boundOrd
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
    random: UnsafeUniformRng,
    domainOps: DomainOps[E, D]
  )(
    validationFunc: OrderValidationFunc[Bound.Upper[E]] = domainOps.boundOrd.strictValidationFunc
  ): OrderedSetFactory[E, D] =
    (bounds, complementary) => fromIterableUnsafe[E, D](bounds, random, complementary, domainOps)(validationFunc)
}