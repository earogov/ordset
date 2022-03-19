package ordset.core.segmentSeq.map

import ordset.core.domain.Domain

/**
 * Unified interface to build ordered maps from collection of interval relations.
 * 
 * @tparam E type of elements on ordered domain
 * @tparam D type of ordered domain
 * @tparam V type of value assigned to range of elements
 * @tparam SSeq type of output ordered set
 */
trait OrderedMapBuilder[E, D[X] <: Domain[X], V, +SSeq <: OrderedMap[E, D, V]] {
  builder =>


}
