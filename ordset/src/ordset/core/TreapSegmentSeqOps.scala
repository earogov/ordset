package ordset.core

import ordset.core.domain.Domain
import AbstractTreapSegmentSeq._
import AbstractUniformSegmentSeq._
import ordset.core.map.TreapOrderedMap

object TreapSegmentSeqOps {

  /**
   * Applies patch operation (see [[SegmentLikeT.patched]]) for given segment of [[TreapSegmentSeq]].
   *
   * This implementation unlike one in [[AbstractTreapSegmentSeq]] guaranties that output will be also a treap
   * segment sequence. Note some performance penalty in case when conversion is required.
   */
  def patchSegment[E, D <: Domain[E], V](
    segment: TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V],
    sequence: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] = 
    segment match {
      case s: TreapSegmentBase[_, _, _] => s.patched(sequence)
      case _: UniformSingleSegment[_, _, _] => TreapOrderedMap.getFactory.convertMap(sequence)
    }
}
