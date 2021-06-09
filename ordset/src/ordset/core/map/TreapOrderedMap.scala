package ordset.core.map

import ordset.core.{Bound, SegmentSeqException, SegmentSeqOps, SeqValidationPredicate}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.random.RngManager
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap
import ordset.util.ValueHolder

import scala.util.control.NonFatal

object TreapOrderedMap {

  /**
   * Creates ordered map from node of treap containing bounds and values.
   *
   * Validation of treap invariants (keys and priorities order) is not applied.
   *
   * @param root       node of treap containing upper bounds and values.
   * @param lastValue  value of last segment.
   * @param domainOps  domain specific typeclasses: elements ordering, etc.
   * @param valueOps   value specific typeclasses: equality, set inclusion function, etc.
   * @param rngManager generator of random sequences.
   */
  def unchecked[E, D <: Domain[E], V](
    root: ImmutableTreap[Bound.Upper[E], V],
    lastValue: V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): TreapOrderedMap[E, D, V] =
    root match {
      case root: ImmutableTreap.Node[Bound.Upper[E], V] => NonuniformTreapOrderedMap.unchecked(root, lastValue)
      case _ => UniformOrderedMap.apply(lastValue, getFactory)
    }

  /**
   * Returns ordered set factory.
   */
  def getFactory[E, D <: Domain[E], V]: OrderedMapFactory[E, D, V, TreapOrderedMap[E, D, V]] =
    factoryInstance.asInstanceOf[OrderedMapFactory[E, D, V, TreapOrderedMap[E, D, V]]]

  // Private section ---------------------------------------------------------- //
  private lazy val factoryInstance: Factory[Any, Domain[Any], Any] = new Factory()

  private class Factory[E, D <: Domain[E], V] extends OrderedMapFactory[E, D, V, TreapOrderedMap[E, D, V]] {

    @throws[SegmentSeqException]("if unable to build valid map with specified bounds and values")
    def unsafeBuildAsc(
      seq: IterableOnce[(Bound.Upper[E], V)],
      domainOps: DomainOps[E, D],
      valueOps: ValueOps[V]
    )(
      boundsValidation: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation,
      valuesValidation: SeqValidationPredicate[V] = valueOps.distinctionValidation
    )(
      implicit rngManager: RngManager
    ): TreapOrderedMap[E, D, V] = {
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
            NonuniformTreapOrderedMap.unchecked(r, lastValue.get)(domainOps, valueOps, rngManager)
          case _ =>
            UniformOrderedMap.apply(lastValue.get, this)(domainOps, valueOps, rngManager)
        }
      } catch {
        case NonFatal(e) => throw SegmentSeqException.seqBuildFailed(e)
      }
    }

    override def convertMap(map: OrderedMap[E, D, V]): TreapOrderedMap[E, D, V] =
      map match {
        case map: TreapOrderedMap[E, D, V] => map
        case _ => convertMapInternal(map)
      }
  }
}
