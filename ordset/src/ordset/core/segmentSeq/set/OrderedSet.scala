package ordset.core.segmentSeq.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.interval.Interval
import scala.util.Try
import ordset.random.RngManager

object OrderedSet {

  /**
   * Builds ordered set or fails, if preconditions are not met (see [[OrderedSetBuilder.tryBuild]] for details).
   * 
   * @tparam E type of elements on ordered domain
   * @tparam D type of ordered domain
   * 
   * @param intervals collection of intervals.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param rngManager generator of random sequences.
   */
  def tryBuild[E, D[X] <: Domain[X]](
    intervals: Iterable[Interval[E, D]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): Try[StrictOrderedSet[E, D]] =
    getBuilder.tryBuild(intervals)
  
  /**
   * Returns ordered set factory (see [[OrderedSetFactory]]).
   * 
   * @tparam E type of elements on ordered domain
   * @tparam D type of ordered domain
   */
  def getFactory[E, D[X] <: Domain[X]]: OrderedSetFactory[E, D, StrictOrderedSet[E, D]] =
    TreapOrderedSet.getFactory

  /**
   * Returns ordered set builder (see [[OrderedSetBuilder]]).
   * 
   * @tparam E type of elements on ordered domain
   * @tparam D type of ordered domain
   */
  def getBuilder[E, D[X] <: Domain[X]]: OrderedSetBuilder[E, D, StrictOrderedSet[E, D]] =
    TreapOrderedSet.getBuilder
}
