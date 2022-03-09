package ordset.core.segmentSeq.set

import ordset.core.Bound
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.SegmentSeqException
import ordset.core.segmentSeq.validation.ValidatingIterable
import ordset.random.RngManager
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap

import scala.util.control.NonFatal

object TreapOrderedSet {

  /**
   * Creates ordered set from treap containing bounds and values.
   *
   * Validation of treap invariants (keys and priorities order) is not applied.
   *
   * @param root treap containing upper bounds and values.
   * @param lastValue value of last segment.
   * @param complementary if `true`, values of all segments are inverted.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  def unchecked[E, D[X] <: Domain[X]](
    root: ImmutableTreap[Bound.Upper[E], Boolean],
    lastValue: Boolean,
    complementary: Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): TreapOrderedSet[E, D] = 
    root match {
      case root: ImmutableTreap.Node[Bound.Upper[E], Boolean] => 
        NonuniformTreapOrderedSet.uncheckedOptimized(root, lastValue, complementary)
      case _ => 
        UniformOrderedSet.apply(complementary ^ lastValue, TreapOrderedSet.getFactory)
    }

  /**
   * Returns ordered set factory (see [[OrderedSetFactory]]).
   */
  def getFactory[E, D[X] <: Domain[X]]: OrderedSetFactory[E, D, TreapOrderedSet[E, D]] =
    factoryInstance.asInstanceOf[OrderedSetFactory[E, D, TreapOrderedSet[E, D]]]

  /**
   * Returns ordered set builder (see [[OrderedSetBuilder]]).
   */
  def getBuilder[E, D[X] <: Domain[X]]: OrderedSetBuilder[E, D, TreapOrderedSet[E, D]] =
    builderInstance.asInstanceOf[OrderedSetBuilder[E, D, TreapOrderedSet[E, D]]]

  // Private section ---------------------------------------------------------- //
  private lazy val factoryInstance: Factory[Any, Domain] = new Factory()

  private lazy val builderInstance: OrderedSetBuilder[Any, Domain, TreapOrderedSet[Any, Domain]] = 
    OrderedSetBuilder.default(factoryInstance)

  private class Factory[E, D[X] <: Domain[X]] extends OrderedSetFactory[E, D, TreapOrderedSet[E, D]] {

    @throws[SegmentSeqException]("if preconditions are violated")
    override def unsafeBuild(
      bounds: ValidatingIterable[Bound.Upper[E]],
      complementary: Boolean
    )(
      implicit 
      domainOps: DomainOps[E, D],
      rngManager: RngManager
    ): TreapOrderedSet[E, D] =
      try {
        val rng = rngManager.newUnsafeUniformRng()
        val boundOrd = domainOps.domain.boundOrd
        var value = complementary

        val buffer =
          bounds.foldLeftValidated(List.empty[MutableTreap.Node[Bound.Upper[E], Boolean]]) { (buf, bnd) =>
            val newBuf =
              BuildAsc.addToBuffer[Bound.Upper[E], Bound[E], Boolean](
                buf, bnd, rng.nextInt(), value
              )(
                boundOrd
              )
            value = !value
            newBuf
          }

        val root = BuildAsc.finalizeBuffer(buffer)
        root match {
          case r: ImmutableTreap.Node[Bound.Upper[E], Boolean] =>
            NonuniformTreapOrderedSet.uncheckedOptimized(r, value, false)(domainOps, rngManager)
          case _ =>
            UniformOrderedSet.apply(value, this)(domainOps, rngManager)
        }
      } catch {
        case NonFatal(e) => throw SegmentSeqException.seqBuildFailed(e)
      }

    override def convertSet(set: OrderedSet[E, D]): TreapOrderedSet[E, D] =
      set match {
        case set: TreapOrderedSet[E, D] => set
        case _ => convertSetInternal(set)
      }
  }
}
