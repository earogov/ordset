package ordset.core

import ordset.core.domain.Domain

object TreapSegmentSeqOps {

  def convertToTreap[E, D <: Domain[E], V](
    sequence: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] = sequence match {
    case seq: TreapSegmentSeq[E, D, V] => seq
    case _ => ???
  }

  def patchSegment[E, D <: Domain[E], V](
    segment: AbstractTreapSegmentSeq.TreapSegment[E, D, V] | AbstractUniformSegmentSeq.UniformSingleSegment[E, D, V],
    sequence: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] = ???
}
