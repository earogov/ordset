package ordset.core.set

import ordset.core.domain.{Domain, DomainOps, OrderValidationFunc}
import ordset.core._
import ordset.random.RngManager
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap

import java.util.NoSuchElementException

class TreapOrderedSet[E, D <: Domain[E]] protected(
  final override val root: ImmutableTreap.Node[Bound.Upper[E], Boolean],
  final override val lastValue: Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractTreapOrderedSet[E, D]
  with OrderedSetCommons[E, D]{

  // Protected section -------------------------------------------------------- //
  protected def consUniform(value: Boolean): OrderedSet[E, D] =
    UniformOrderedSet.apply(value)

  protected def consFromNode(
    node: ImmutableTreap.Node[Bound.Upper[E], Boolean],
    value: Boolean
  ): OrderedSet[E, D] =
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
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): OrderedSet[E, D] = new TreapOrderedSet(root, lastValue)

  /**
   * Creates ordered set from collection of upper bounds.
   *
   * Preconditions:
   *
   * 1. `bounds` collection is ordered according to `validationFunc`:
   * <tr>
   *   validationFunc(bounds^i-1^, bounds^i^) == true for each i in [1, bounds.size]
   * </tr>
   * <tr></tr>
   * 
   * Method is considered 'unsafe' because it throws exception if preconditions are violated.
   *
   * @param bounds collection of upper bounds.
   * @param complementary when `true`: first segment is included in set, second - excluded, etc;
   *                      when `false`: first segment is excluded, second - included, etc.
   * @param domainOps domain related functions.
   * @param validationFunc validates ordering of `bounds`.
   * @param rngManager provides random numbers generator.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def fromIterableUnsafe[E, D <: Domain[E]](
    bounds: IterableOnce[Bound.Upper[E]],
    complementary: Boolean,
    domainOps: DomainOps[E, D]
  )(
    validationFunc: OrderValidationFunc[Bound.Upper[E]] = domainOps.boundOrd.strictValidationFunc,
  )(
    implicit rngManager: RngManager
  ): OrderedSet[E, D] = {
    val rng = rngManager.newUnsafeUniformRng()
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
              BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], Boolean](
                buf, bnd, rng.nextInt(), value
              )(
                boundOrd
              )
            value = !value
            buffer
          }
        )
      val root = BuildAsc.finalizeBuffer(buffer)
      root match {
        case r: ImmutableTreap.Node[Bound.Upper[E], Boolean] => new TreapOrderedSet(r, value)(domainOps, rngManager)
        case _ => UniformOrderedSet(value)(domainOps, rngManager)
      }
    } catch {
      case e @ (_: NoSuchElementException | _: IllegalArgumentException) => throw SegmentSeqException(e)
    }
  }

  /**
   * Returns ordered set factory.
   */
  def getFactory[E, D <: Domain[E]](
    domainOps: DomainOps[E, D]
  )(
    validationFunc: OrderValidationFunc[Bound.Upper[E]] = domainOps.boundOrd.strictValidationFunc
  )(
    implicit rngManager: RngManager
  ): OrderedSetFactory[E, D] =
    (bounds, complementary) => fromIterableUnsafe[E, D](bounds, complementary, domainOps)(validationFunc)(rngManager)
}