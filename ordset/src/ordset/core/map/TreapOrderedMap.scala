package ordset.core.map

import ordset.core._
import ordset.core.domain.{Domain, DomainOps, OrderValidationFunc}
import ordset.core.value.ValueOps
import ordset.random.RngManager
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap

import java.util.NoSuchElementException

class TreapOrderedMap[E, D <: Domain[E], V] protected(
  final override val root: ImmutableTreap.Node[Bound.Upper[E], V],
  final override val lastValue: V
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val valueOps: ValueOps[V],
  final override val rngManager: RngManager
) extends AbstractTreapSegmentSeq[E, D, V]
  with OrderedMapCommons[E, D, V]{

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: V): UniformOrderedMap[E, D, V] =
    UniformOrderedMap.apply(value)

  @inline
  protected final override def consFromNode(
    node: ImmutableTreap.Node[Bound.Upper[E], V],
    value: V
  ): TreapOrderedMap[E, D, V] =
    TreapOrderedMap.unchecked(node, value)

  @inline
  protected final override def isValueIncluded(value: V): Boolean = valueOps.isIncluded(value)
}

object TreapOrderedMap {

  /**
   * Creates ordered map from treap node (see [[AbstractSegmentSeq]]).
   *
   * Validation of treap invariants (keys and priorities order) is not applied.
   */
  def unchecked[E, D <: Domain[E], V](
    root: ImmutableTreap.Node[Bound.Upper[E], V],
    lastValue: V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): TreapOrderedMap[E, D, V] =
    new TreapOrderedMap(root, lastValue)

  /**
   * Creates ordered map from collections of upper bounds and values.
   *
   * Preconditions:
   *
   * 1. `bounds` collection is ordered according to `validationFunc`:
   * <tr>
   *   validationFunc(bounds,,i-1,,, bounds,,i,,) == true for each i in [1, bounds.size - 1]
   * </tr>
   * 
   * 2. `values` collection size is greater by 1 then `bounds` size:
   * <tr>
   *   values.size == bounds.size + 1
   * </tr>
   * <tr></tr>
   *
   * Method is considered 'unsafe' because it throws exception if preconditions are violated.
   *
   * @param bounds collection of upper bounds.
   * @param values collection of values.
   * @param domainOps domain related functions.
   * @param validationFunc validates ordering of `bounds`.
   * @param rngManager provides random numbers generator.
   * @param valueOps value related functions (equality and inclusion typeclasses, etc)
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def fromIterableUnsafe[E, D <: Domain[E], V](
    bounds: IterableOnce[Bound.Upper[E]],
    values: IterableOnce[V],
    domainOps: DomainOps[E, D]
  )(
    validationFunc: OrderValidationFunc[Bound.Upper[E]] = domainOps.boundOrd.strictValidationFunc,
  )(
    implicit
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): TreapSegmentSeq[E, D, V] = {
    val rng = rngManager.newUnsafeUniformRng()
    val boundOrd = domainOps.domain.boundOrd
    val valuesIter = values.iterator
    try {
      val buffer =
        OrderValidationFunc.foldIterableAfter[Bound.Upper[E], List[MutableTreap.Node[Bound.Upper[E], V]]](
          bounds,
          validationFunc,
          List.empty[MutableTreap.Node[Bound.Upper[E], V]],
          (buf, bnd) =>
            BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], V](
              buf, bnd, rng.nextInt(), valuesIter.next()
            )(
              boundOrd
            )
        )
      val root = BuildAsc.finalizeBuffer(buffer)
      root match {
        case r: ImmutableTreap.Node[Bound.Upper[E], V] =>
          TreapOrderedMap.unchecked(r, valuesIter.next())(domainOps, valueOps, rngManager)
        case _ =>
          UniformOrderedMap(valuesIter.next())(domainOps, valueOps, rngManager)
      }
    } catch {
      case e @ (_: NoSuchElementException | _: IllegalArgumentException) => throw SegmentSeqException(e)
    }
  }

  /**
   * Returns ordered map factory.
   */
  def getFactory[E, D <: Domain[E], V](
    domainOps: DomainOps[E, D]
  )(
    validationFunc: OrderValidationFunc[Bound.Upper[E]] = domainOps.boundOrd.strictValidationFunc
  )(
    implicit
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): OrderedMapFactory[E, D, V] =
    (bounds, values) => fromIterableUnsafe[E, D, V](bounds, values, domainOps)(validationFunc)(valueOps, rngManager)
}