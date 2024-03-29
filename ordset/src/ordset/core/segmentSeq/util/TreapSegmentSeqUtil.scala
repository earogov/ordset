package ordset.core.segmentSeq.util

import ordset.core.{Bound, ExtendedBound}
import ordset.core.segmentSeq._
import ordset.core.segmentSeq.AbstractTreapSegmentSeq.{TreapSegmentBase => NonuniformTreapSegmentBase}
import ordset.core.domain.Domain
import ordset.core.segmentSeq.map.TreapOrderedMap
import ordset.tree.treap.immutable.ImmutableTreap

object TreapSegmentSeqUtil {

  /**
   * Get treap root node if sequence is non-uniform or empty treap otherwise.
   */
  def getRoot[E, D[X] <: Domain[X], V](sequence: TreapSegmentSeq[E, D, V]): ImmutableTreap[Bound.Upper[E], V] =
    sequence match {
      case s: NonuniformTreapSegmentSeq[E, D, V] => s.root
      case _: UniformSegmentSeq[E, D, V] => ImmutableTreap.Empty
    }

  /**
   * Applies [[SegmentLikeT.takeBelow]] operation to the given segment of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeBelowSegment[E, D[X] <: Domain[X], V](
    segment: TreapSegmentBase[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    // We have to pattern match here due to type inference issue.
    segment match {
      case s: NonuniformTreapSegmentBase[E, D, V] => s.takeBelow
      case s: UniformSegment[E, D, V] => s.takeBelow
    }

  /**
   * Applies [[SegmentSeqT.takeBelowBound]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeBelowBound[E, D[X] <: Domain[X], V](
    sequence: SegmentSeqT[E, D, V, TreapSegmentBase[E, D, V]],
    bound: Bound[E]
  ): TreapSegmentSeq[E, D, V] =
    takeBelowSegment(sequence.getSegmentForBound(bound))

  /**
   * Applies [[SegmentSeqT.takeBelowExtended]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeBelowExtended[E, D[X] <: Domain[X], V](
    sequence: SegmentSeqT[E, D, V, TreapSegmentBase[E, D, V]],
    bound: ExtendedBound[E]
  ): TreapSegmentSeq[E, D, V] =
    takeBelowSegment(sequence.getSegmentForExtended(bound))

  /**
   * Applies [[SegmentLikeT.takeAbove]] operation to the given segment of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeAboveSegment[E, D[X] <: Domain[X], V](
    segment: TreapSegmentBase[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    // We have to pattern match here due to type inference issue.
    segment match {
      case s: NonuniformTreapSegmentBase[E, D, V] => s.takeAbove
      case s: UniformSegment[E, D, V] => s.takeAbove
    }

  /**
   * Applies [[SegmentSeqT.takeAboveBound]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeAboveBound[E, D[X] <: Domain[X], V](
    sequence: SegmentSeqT[E, D, V, TreapSegmentBase[E, D, V]],
    bound: Bound[E]
  ): TreapSegmentSeq[E, D, V] =
    takeAboveSegment(sequence.getSegmentForBound(bound))

  /**
   * Applies [[SegmentSeqT.takeAboveExtended]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeAboveExtended[E, D[X] <: Domain[X], V](
    sequence: SegmentSeqT[E, D, V, TreapSegmentBase[E, D, V]],
    bound: ExtendedBound[E]
  ): TreapSegmentSeq[E, D, V] =
    takeAboveSegment(sequence.getSegmentForExtended(bound))

  /**
   * Applies [[SegmentLikeT.slice]] operation to the given segment of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def sliceSegment[E, D[X] <: Domain[X], V](
    segment: TreapSegmentBase[E, D, V]
  ): (TreapSegmentSeq[E, D, V], TreapSegmentSeq[E, D, V]) =
    // We have to pattern match here due to type inference issue.
    segment match {
      case s: NonuniformTreapSegmentBase[E, D, V] => s.slice
      case s: UniformSegment[E, D, V] => s.slice
    }

  /**
   * Applies [[SegmentSeqT.sliceAtBound]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def sliceAtBound[E, D[X] <: Domain[X], V](
    sequence: SegmentSeqT[E, D, V, TreapSegmentBase[E, D, V]],
    bound: Bound[E]
  ): (TreapSegmentSeq[E, D, V], TreapSegmentSeq[E, D, V]) =
    sliceSegment(sequence.getSegmentForBound(bound))

  /**
   * Applies [[SegmentSeqT.sliceAtExtended]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def sliceAtExtended[E, D[X] <: Domain[X], V](
    sequence: SegmentSeqT[E, D, V, TreapSegmentBase[E, D, V]],
    bound: ExtendedBound[E]
  ): (TreapSegmentSeq[E, D, V], TreapSegmentSeq[E, D, V]) =
    sliceSegment(sequence.getSegmentForExtended(bound))

  /**
   * Applies [[SegmentT.prepend]] operation to the given segment of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   * Note some performance penalty in case when conversion is required.
   */
  def prependBelowSegment[E, D[X] <: Domain[X], V](
    segment: SegmentT[E, D, V, TreapSegmentBase[E, D, V]],
    otherSeq: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    segment.prepend(otherSeq) match {
      case seq: TreapSegmentSeq[E, D, V] => seq
      case seq => TreapOrderedMap.getFactory.convertMap(seq)
    }
  
  /**
   * Applies [[SegmentTruncationT.prepend]] operation to the given truncation of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   * Note some performance penalty in case when conversion is required.
   */
  def prependBelowTruncation[E, D[X] <: Domain[X], V](
    truncation: SegmentTruncationT[E, D, V, TreapSegmentBase[E, D, V], SegmentT[E, D, V, TreapSegmentBase[E, D, V]]],
    otherSeq: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    truncation.prepend(otherSeq) match {
      case seq: TreapSegmentSeq[E, D, V] => seq
      case seq => TreapOrderedMap.getFactory.convertMap(seq)
    }

  /**
   * Applies [[SegmentT.append]] operation to the given segment of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   * Note some performance penalty in case when conversion is required.
   */
  def appendAboveSegment[E, D[X] <: Domain[X], V](
    segment: SegmentT[E, D, V, TreapSegmentBase[E, D, V]],
    otherSeq: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    segment.append(otherSeq) match {
      case seq: TreapSegmentSeq[E, D, V] => seq
      case seq => TreapOrderedMap.getFactory.convertMap(seq)
    }
  
  /**
   * Applies [[SegmentTruncationT.append]] operation to the given truncation of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   * Note some performance penalty in case when conversion is required.
   */
  def appendAboveTruncation[E, D[X] <: Domain[X], V](
    truncation: SegmentTruncationT[E, D, V, TreapSegmentBase[E, D, V], SegmentT[E, D, V, TreapSegmentBase[E, D, V]]],
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
  def patchSegment[E, D[X] <: Domain[X], V](
    segment: TreapSegmentBase[E, D, V],
    otherSeq: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    segment match {
      case s: NonuniformTreapSegmentBase[E, D, V] => s.patch(otherSeq)
      case _: UniformSegment[E, D, V] => TreapOrderedMap.getFactory.convertMap(otherSeq)
    }
}
