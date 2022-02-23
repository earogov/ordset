package ordset.core.segmentSeq

import ordset.core.domain.Domain

/**
 * Base class for strict segment sequences (see [[StrictSegmentSeqT]]).
 * 
 * @tparam E type of elements on ordered domain
 * @tparam D type of ordered domain
 * @tparam V type of value assigned to range of elements
 * @tparam S type of range state
 */
abstract class AbstractStrictSegmentSeq[E, D[X] <: Domain[X], V, +S] 
  extends AbstractSegmentSeq[E, D, V, S]
  with StrictSegmentSeqT[E, D, V, S]
