package ordset.core

import ordset.core.domain.Domain

package object map {

  type OrderedMap[E, D <: Domain[E], V] = SegmentSeq[E, D, V]

  type OrderedMapT[E, D <: Domain[E], V, +S] = SegmentSeqT[E, D, V, S]
}
