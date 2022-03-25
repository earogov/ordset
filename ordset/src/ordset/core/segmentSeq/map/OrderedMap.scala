package ordset.core.segmentSeq.map

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.interval.IntervalRelation
import ordset.random.RngManager
import scala.util.Try

object OrderedMap {
  
  /**
   * Builds ordered map or fails, if preconditions are not met (see [[OrderedMapBuilder.tryBuild]] for details).
   * 
   * @tparam E type of elements on ordered domain
   * @tparam D type of ordered domain
   * @tparam V type of value assigned to range of elements
   * 
   * @param defaultValue value assigned to elements that are not included in specified intervals.
   * @param intervals collection of intervals.
   * @param domainOps domain specific typeclasses: elements ordering, etc.
   * @param valueOps value specific typeclasses: equality, set inclusion function, etc. 
   * @param rngManager generator of random sequences.
   */
  def tryBuild[E, D[X] <: Domain[X], V](
    defaultValue: V,
    intervals: Iterable[IntervalRelation[E, D, V]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): Try[StrictOrderedMap[E, D, V]] =
    getBuilder.tryBuild(defaultValue, intervals)

  /**
   * Returns ordered map factory (see [[OrderedMapFactory]]).
   * 
   * @tparam E type of elements on ordered domain
   * @tparam D type of ordered domain
   * @tparam V type of value assigned to range of elements
   */
  def getFactory[E, D[X] <: Domain[X], V]: OrderedMapFactory[E, D, V, StrictOrderedMap[E, D, V]] =
    TreapOrderedMap.getFactory

  /**
   * Returns ordered map builder (see [[OrderedMapBuilder]]).
   * 
   * @tparam E type of elements on ordered domain
   * @tparam D type of ordered domain
   * @tparam V type of value assigned to range of elements
   */
  def getBuilder[E, D[X] <: Domain[X], V]: OrderedMapBuilder[E, D, V, StrictOrderedMap[E, D, V]] =
    TreapOrderedMap.getBuilder
}
