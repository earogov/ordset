package ordset.core.map

import ordset.core
import ordset.core.{Bound, ExtendedBound, SegmentSeqException, SeqValidationPredicate}
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

    @throws[SegmentSeqException]("if preconditions are violated")
    def unsafeBuildAsc(
      seq: IterableOnce[(ExtendedBound.Upper[E], V)],
      domainOps: DomainOps[E, D],
      valueOps: ValueOps[V]
    )(
      boundsValidation: SeqValidationPredicate[ExtendedBound.Upper[E]] = domainOps.extendedOrd.strictValidation,
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
        var prevItem: (ExtendedBound.Upper[E], V) = null
        while (iter.hasNext) {
          if (root != null) {
            throw new IllegalArgumentException(
              s"Item with bound == ${ExtendedBound.AboveAll} must be last in input sequence"
            )
          }
          val item = iter.next()
          val bound = item._1
          val value = item._2
          // Validation
          if (prevItem != null) {
            boundsValidation.validate(prevItem._1, bound)
            valuesValidation(prevItem._2, value)
          }
          // Building
          bound match {
            case bound: Bound.Upper[E] =>
              buffer = BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], V](
                buffer, bound, rng.nextInt(), value
              )(
                boundOrd
              )
            case ExtendedBound.AboveAll =>
              root = BuildAsc.finalizeBuffer(buffer)
              lastValue.set(value)
          }
          prevItem = item
        }
        if (root == null) {
          throw new IllegalArgumentException(
            s"Last item in input sequence must have bound == ${ExtendedBound.AboveAll}"
          )
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
