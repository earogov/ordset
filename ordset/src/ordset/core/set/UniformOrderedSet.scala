package ordset.core.set

import ordset.core.{AbstractUniformSegmentSeq, Bound, SegmentSeq, OrderedSet}
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager

class UniformOrderedSet[E, D <: Domain[E]](
  final override val value: Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractUniformSegmentSeq[E, D, Boolean]
  with OrderedSetCommons[E, D] {

  // Transformation ----------------------------------------------------------- //
  // TODO implement `appended`
  final override def appended(bound: Bound[E], other: OrderedSet[E, D]): OrderedSet[E, D] = {
    // original:
    //                bound
    //                   )
    // X---------------false------------------X
    //
    // other:
    //                      otherBoundSegment
    //                       /
    // X--f--)[--------true------](---false---X
    //
    // original.appended(bound, other):
    //
    // X-------false-----)[--tr--](---false---X
    //                   ^
    //                 bound
    val lowerBound = bound.provideLower
    val otherBoundSegment = other.getSegment(lowerBound)
    if (valueOps.eqv(value, otherBoundSegment.value)) {
      ???
    } else {
      ???
    }
  }

  // Protected section -------------------------------------------------------- //  
  @inline
  protected final override def isIncludedInSet(value: Boolean): Boolean = value
}

object UniformOrderedSet {

  def apply[E, D <: Domain[E]](
    value: Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](value)

  def empty[E, D <: Domain[E]](
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](false)

  def universal[E, D <: Domain[E]](
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): UniformOrderedSet[E, D] =
    new UniformOrderedSet[E, D](true)
}
