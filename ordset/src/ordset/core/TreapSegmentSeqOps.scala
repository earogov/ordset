package ordset.core

import ordset.core.domain.Domain
import AbstractTreapSegmentSeq._
import AbstractUniformSegmentSeq._
import ordset.core.map.TreapOrderedMap
import ordset.tree.treap.immutable.ImmutableTreap

object TreapSegmentSeqOps {

  /**
   * Get treap root node if sequence is non-uniform or empty treap otherwise.
   */
  def getRoot[E, D <: Domain[E], V](seq: TreapSegmentSeq[E, D, V]): ImmutableTreap[Bound.Upper[E], V] =
    seq match {
      case s: NonuniformTreapSegmentSeq[E, D, V] => s.root
      case _: UniformSegmentSeq[E, D, V] => ImmutableTreap.Empty
    }
  
  /**
   * Applies patch operation (see [[SegmentLikeT.patch]]) for given segment of [[TreapSegmentSeq]].
   *
   * This implementation unlike one in [[AbstractTreapSegmentSeq]] guaranties that output will be also a treap
   * segment sequence. Note some performance penalty in case when conversion is required.
   */
  def patchSegment[E, D <: Domain[E], V](
    segment: TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V],
    sequence: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] = 
    segment match {
      case s: TreapSegmentBase[_, _, _] => s.patch(sequence)
      case _: UniformSingleSegment[_, _, _] => TreapOrderedMap.getFactory.convertMap(sequence)
    }
}
