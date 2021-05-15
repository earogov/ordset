package ordset.core.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.core._
import ordset.random.RngManager
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.transform.BuildAsc
import ordset.tree.treap.mutable.MutableTreap

import java.util.NoSuchElementException
import scala.util.control.NonFatal

class TreapOrderedSet[E, D <: Domain[E]] protected(
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
    UniformOrderedSet.apply(value)

  @inline
  protected final override def consFromNode(
    node: ImmutableTreap.Node[Bound.Upper[E], Boolean],
    value: Boolean
  ): TreapOrderedSet[E, D] =
    TreapOrderedSet.unchecked(node, value)

  @inline
  protected final override def isValueIncluded(value: Boolean): Boolean = value
}

object TreapOrderedSet {

  /**
   * Creates ordered set from treap node (see [[AbstractSegmentSeq]]).
   *
   * Validation of treap invariants (keys and priorities order) is not applied.
   */
  def unchecked[E, D <: Domain[E]](
    root: ImmutableTreap.Node[Bound.Upper[E], Boolean],
    lastValue: Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): TreapOrderedSet[E, D] = 
    new TreapOrderedSet(root, lastValue)

  /**
   * Creates ordered set from collection of upper bounds. See [[OrderedSetFactory.unsafeBuildAsc]] for details.
   *
   * Precondition 1 of [[OrderedSetFactory.unsafeBuildAsc]] is checked by `boundsValidation` function which
   * is applied to each pair of adjacent bounds. [[SegmentSeqException]] is thrown in case of failure.
   */
  @throws[SegmentSeqException]("if preconditions are violated")
  def unsafeBuildAsc[E, D <: Domain[E]](
    bounds: IterableOnce[Bound.Upper[E]],
    complementary: Boolean,
    domainOps: DomainOps[E, D]
  )(
    boundsValidation: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation,
  )(
    implicit rngManager: RngManager
  ): OrderedSet[E, D] = {
    try {
      val rng = rngManager.newUnsafeUniformRng()
      val boundOrd = domainOps.domain.boundOrd
      var value = complementary
      val buffer =
        SeqValidationPredicate.foldIterableAfter[Bound.Upper[E], List[MutableTreap.Node[Bound.Upper[E], Boolean]]](
          bounds,
          boundsValidation,
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
        case r: ImmutableTreap.Node[Bound.Upper[E], Boolean] =>
          TreapOrderedSet.unchecked(r, value)(domainOps, rngManager)
        case _ =>
          UniformOrderedSet(value)(domainOps, rngManager)
      }
    } catch {
      case NonFatal(e) => throw SegmentSeqException.seqBuildFailed(e)
    }
  }

  /**
   * Returns ordered set factory. Implementation is based on [[unsafeBuildAsc]].
   */
  def getFactory[E, D <: Domain[E]](
    domainOps: DomainOps[E, D]
  )(
    boundsValidation: SeqValidationPredicate[Bound.Upper[E]] = domainOps.boundOrd.strictValidation
  )(
    implicit rngManager: RngManager
  ): OrderedSetFactory[E, D] =
    (bounds, complementary) => unsafeBuildAsc(bounds, complementary, domainOps)(boundsValidation)(rngManager)
}