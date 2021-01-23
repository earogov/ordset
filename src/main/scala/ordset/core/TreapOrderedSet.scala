package ordset.core

import ordset.core.domain.{Domain, DomainOps}
import ordset.tree.core.Validation
import ordset.tree.treap.immutable.ImmutableTreap
import ordset.tree.treap.immutable.fold.BuildAsc
import ordset.tree.treap.mutable.MutableTreap
import ordset.util

class TreapOrderedSet[E, D <: Domain[E]](
  final override val root: ImmutableTreap.Node[Bound.Upper[E], Boolean],
  final override val lastValue: Boolean
)(
  implicit final override val domainOps: DomainOps[E, D]
) extends AbstractTreapSegmentSeq[E, D, Boolean] {

  @inline
  protected final override def isIncludedInSet(value: Boolean): Boolean = value
}

object TreapOrderedSet {

  /**
   * Creates ordered set from bounds collection.
   */
  @throws[NoSuchElementException]("if size of priorities collection is less than size of bounds collection")
  @throws[AssertionError]("if key order validation (with validationFunc) fails")
  def fromIterable[E, D <: Domain[E]](
    bounds: IterableOnce[Bound.Upper[E]],
    priorities: IterableOnce[Int],
    complementary: Boolean,
    validationFunc: Validation.KeyOrderFunc[Bound.Upper[E]]
  )(
    implicit domainOps: DomainOps[E, D],

  ): OrderedSet[E, D] = {
    var buffer = List.empty[MutableTreap.Node[Bound.Upper[E], Boolean]]
    var value = complementary
    var prevBound: Bound.Upper[E] = null
    val boundOrd = domainOps.domain.boundOrd
    val intOrd = domainOps.domain.intOrd
    val boundIterator = bounds.iterator
    val priorityIterator = priorities.iterator
    while(boundIterator.hasNext) {
      val bound = boundIterator.next()
      if (prevBound != null && !validationFunc(prevBound, bound)) {
        throw new AssertionError(s"Illegal key order: $prevBound >= $bound")
      }
      val priority = util.IterableUtil.nextOrThrowMsg(
        priorityIterator,
        "Size of priorities collection must be greater or equal to size of bounds collection."
      )
      buffer = BuildAsc.appendToBuffer[Bound.Upper[E], Bound[E], Boolean](
        buffer, bound, priority, value
      )(
        intOrd, boundOrd
      )
      value = !value
      prevBound = bound
    }
    val root = BuildAsc.finalizeBuffer(buffer)
    root match {
      case r: ImmutableTreap.Node[Bound.Upper[E], Boolean] => new TreapOrderedSet(r, value)(domainOps)
      case _ => UniformOrderedSet(value)(domainOps)
    }
  }
}