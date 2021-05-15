package ordset.core.map

import ordset.core._
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.random.RngManager
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap

import java.util.NoSuchElementException
import scala.util.control.NonFatal

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
   * Creates ordered map from treap node.
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
   * Creates ordered map from collections of upper bounds and values. See [[OrderedMapFactory.unsafeBuildAsc]]
   * for details.
   *
   * Precondition 3 of [[OrderedMapFactory.unsafeBuildAsc]] is checked by `boundsValidation` function which
   * is applied to each pair of adjacent bounds (except last bound == null). [[SegmentSeqException]] is thrown
   * in case of failure.
   *
   * Precondition 4 of [[OrderedMapFactory.unsafeBuildAsc]] is checked by `valuesValidation` function which
   * is applied to each pair of adjacent values.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuildAsc[E, D <: Domain[E], V](
    seq: IterableOnce[(Bound.Upper[E], V)],
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  )(
    boundsValidation: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation,
    valuesValidation: SeqValidationPredicate[V] = valueOps.distinctionValidation
  )(
    implicit rngManager: RngManager,
  ): TreapSegmentSeq[E, D, V] = {
    try {
      val rng = rngManager.newUnsafeUniformRng()
      val boundOrd = domainOps.domain.boundOrd
      val iter = seq.iterator
      val lastValue = new ValueHolder[V]()
      var buffer: List[MutableTreap.Node[Bound.Upper[E], V]] = Nil
      var root: ImmutableTreap[Bound.Upper[E], V] = null
      var prevItem: (Bound.Upper[E], V) = null
      while (iter.hasNext) {
        if (root != null) {
          throw new IllegalArgumentException("Item with bound == null must be last in input sequence")
        }
        val item = iter.next()

        // Validation
        if (prevItem != null) {
          // Exclude last item with bound == null.
          if (item._1 != null) boundsValidation.validate(prevItem._1, item._1)
          valuesValidation(prevItem._2, item._2)
        }
        // Building
        if (item._1 != null) {
          buffer = BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], V](
            buffer, item._1, rng.nextInt(), item._2
          )(
            boundOrd
          )
        } else {
          root = BuildAsc.finalizeBuffer(buffer)
          lastValue.set(item._2)
        }
        prevItem = item
      }
      if (root == null) {
        throw new IllegalArgumentException("Last item in input sequence must have bound == null")
      }
      root match {
        case r: ImmutableTreap.Node[Bound.Upper[E], V] =>
          TreapOrderedMap.unchecked(r, lastValue.get)(domainOps, valueOps, rngManager)
        case _ =>
          UniformOrderedMap(lastValue.get)(domainOps, valueOps, rngManager)
      }
    } catch {
      case NonFatal(e) => throw SegmentSeqException.seqBuildFailed(e)
    }
  }

  private class ValueHolder[V]{
    private var value: V = _

    def get: V = value

    def set(v: V): Unit = {
      value = v
    }
  }

  /**
   * Returns ordered map factory. Implementation is based on [[unsafeBuildAsc]].
   */
  def getFactory[E, D <: Domain[E], V](
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  )(
    boundsValidationFunc: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation,
    valuesValidationFunc: SeqValidationPredicate[V] = valueOps.distinctionValidation
  )(
    implicit
    rngManager: RngManager
  ): OrderedMapFactory[E, D, V] =
    seq => unsafeBuildAsc(seq, domainOps, valueOps)(boundsValidationFunc, valuesValidationFunc)(rngManager)
}