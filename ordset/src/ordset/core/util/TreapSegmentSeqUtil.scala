package ordset.core.util

import ordset.core.AbstractTreapSegmentSeq.TreapSegmentBase
import ordset.core.AbstractUniformSegmentSeq.UniformSingleSegment
import ordset.core.domain.Domain
import ordset.core.map.TreapOrderedMap
import ordset.core._
import ordset.tree.treap.immutable.ImmutableTreap

object TreapSegmentSeqUtil {

  /**
   * Get treap root node if sequence is non-uniform or empty treap otherwise.
   */
  def getRoot[E, D <: Domain[E], V](seq: TreapSegmentSeq[E, D, V]): ImmutableTreap[Bound.Upper[E], V] =
    seq match {
      case s: NonuniformTreapSegmentSeq[E, D, V] => s.root
      case _: UniformSegmentSeq[E, D, V] => ImmutableTreap.Empty
    }

  /**
   * Applies [[SegmentLikeT.takeBelow]] operation to the given segment of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeBelow[E, D <: Domain[E], V](
    segment: TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    // We have to pattern match here due to type inference issue.
    segment match {
      case s: TreapSegmentBase[_, _, _] => s.takeBelow
      case s: UniformSingleSegment[_, _, _] => s.takeBelow
    }

  /**
   * Applies [[SegmentLikeT.takeAbove]] operation to the given segment of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeAbove[E, D <: Domain[E], V](
    segment: TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    // We have to pattern match here due to type inference issue.
    segment match {
      case s: TreapSegmentBase[_, _, _] => s.takeAbove
      case s: UniformSingleSegment[_, _, _] => s.takeAbove
    }

  /**
   * Applies [[SegmentTruncationT.prepend]] operation to the given truncation of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   * Note some performance penalty in case when conversion is required.
   */
  def prependBelowTruncation[E, D <: Domain[E], V](
    truncation: SegmentTruncationT[
      E,
      D,
      V,
      TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V],
      SegmentT[E, D, V, TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V]]
    ],
    otherSeq: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    truncation.prepend(otherSeq) match {
      case seq: TreapSegmentSeq[E, D, V] => seq
      case seq => TreapOrderedMap.getFactory.convertMap(seq)
    }
  
  /**
   * Applies [[SegmentTruncationT.append]] operation to the given truncation of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   * Note some performance penalty in case when conversion is required.
   */
  def appendAboveTruncation[E, D <: Domain[E], V](
    truncation: SegmentTruncationT[
      E,
      D,
      V,
      TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V],
      SegmentT[E, D, V, TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V]]
    ],
    otherSeq: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    truncation.append(otherSeq) match {
      case seq: TreapSegmentSeq[E, D, V] => seq
      case seq => TreapOrderedMap.getFactory.convertMap(seq)
    }

  /**
   * Applies [[SegmentLikeT.patch]] operation to the given segment of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   * Note some performance penalty in case when conversion is required.
   */
  def patchSegment[E, D <: Domain[E], V](
    segment: TreapSegmentBase[E, D, V] | UniformSingleSegment[E, D, V],
    otherSeq: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    segment match {
      case s: TreapSegmentBase[_, _, _] => s.patch(otherSeq)
      case _: UniformSingleSegment[_, _, _] => TreapOrderedMap.getFactory.convertMap(otherSeq)
    }
}
