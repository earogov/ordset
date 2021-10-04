package ordset.core.util

import ordset.core._
import ordset.core.AbstractTreapSegmentSeq.{TreapSegmentBase => NonuniformTreapSegmentBase}
import ordset.core.domain.Domain
import ordset.core.map.TreapOrderedMap
import ordset.tree.treap.immutable.ImmutableTreap

object TreapSegmentSeqUtil {

  /**
   * Get treap root node if sequence is non-uniform or empty treap otherwise.
   */
  def getRoot[E, D <: Domain[E], V](sequence: TreapSegmentSeq[E, D, V]): ImmutableTreap[Bound.Upper[E], V] =
    sequence match {
      case s: NonuniformTreapSegmentSeq[E, D, V] => s.root
      case _: UniformSegmentSeq[E, D, V] => ImmutableTreap.Empty
    }

  /**
   * Applies [[SegmentLikeT.takeBelow]] operation to the given segment of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeBelowSegment[E, D <: Domain[E], V](
    segment: TreapSegmentBase[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    // We have to pattern match here due to type inference issue.
    segment match {
      case s: NonuniformTreapSegmentBase[_, _, _] => s.takeBelow
      case s: UniformSegment[_, _, _] => s.takeBelow
    }

  /**
   * Applies [[SegmentSeqT.takeBelowBound]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeBelowBound[E, D <: Domain[E], V](
    sequence: SegmentSeqT[E, D, V, TreapSegmentBase[E, D, V]],
    bound: Bound[E]
  ): TreapSegmentSeq[E, D, V] =
    takeBelowSegment(sequence.getSegmentForBound(bound))

  /**
   * Applies [[SegmentSeqT.takeBelowExtended]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeBelowExtended[E, D <: Domain[E], V](
    sequence: SegmentSeqT[E, D, V, TreapSegmentBase[E, D, V]],
    bound: ExtendedBound[E]
  ): TreapSegmentSeq[E, D, V] =
    takeBelowSegment(sequence.getSegmentForExtended(bound))

  /**
   * Applies [[SegmentLikeT.takeAbove]] operation to the given segment of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeAboveSegment[E, D <: Domain[E], V](
    segment: TreapSegmentBase[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    // We have to pattern match here due to type inference issue.
    segment match {
      case s: NonuniformTreapSegmentBase[_, _, _] => s.takeAbove
      case s: UniformSegment[_, _, _] => s.takeAbove
    }

  /**
   * Applies [[SegmentSeqT.takeAboveBound]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeAboveBound[E, D <: Domain[E], V](
    sequence: SegmentSeqT[E, D, V, TreapSegmentBase[E, D, V]],
    bound: Bound[E]
  ): TreapSegmentSeq[E, D, V] =
    takeAboveSegment(sequence.getSegmentForBound(bound))

  /**
   * Applies [[SegmentSeqT.takeAboveExtended]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def takeAboveExtended[E, D <: Domain[E], V](
    sequence: SegmentSeqT[E, D, V, TreapSegmentBase[E, D, V]],
    bound: ExtendedBound[E]
  ): TreapSegmentSeq[E, D, V] =
    takeAboveSegment(sequence.getSegmentForExtended(bound))

  /**
   * Applies [[SegmentLikeT.slice]] operation to the given segment of [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def sliceSegment[E, D <: Domain[E], V](
    segment: TreapSegmentBase[E, D, V]
  ): (TreapSegmentSeq[E, D, V], TreapSegmentSeq[E, D, V]) =
    // We have to pattern match here due to type inference issue.
    segment match {
      case s: NonuniformTreapSegmentBase[_, _, _] => s.slice
      case s: UniformSegment[_, _, _] => s.slice
    }

  /**
   * Applies [[SegmentSeqT.sliceAtBound]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def sliceAtBound[E, D <: Domain[E], V](
    sequence: SegmentSeqT[E, D, V, TreapSegmentBase[E, D, V]],
    bound: Bound[E]
  ): (TreapSegmentSeq[E, D, V], TreapSegmentSeq[E, D, V]) =
    sliceSegment(sequence.getSegmentForBound(bound))

  /**
   * Applies [[SegmentSeqT.sliceAtExtended]] operation to the given [[TreapSegmentSeq]].
   *
   * Implementation guaranties that output is also a treap segment sequence.
   */
  def sliceAtExtended[E, D <: Domain[E], V](
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
  def prependBelowSegment[E, D <: Domain[E], V](
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
  def prependBelowTruncation[E, D <: Domain[E], V](
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
  def appendAboveSegment[E, D <: Domain[E], V](
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
  def appendAboveTruncation[E, D <: Domain[E], V](
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
  def patchSegment[E, D <: Domain[E], V](
    segment: TreapSegmentBase[E, D, V],
    otherSeq: SegmentSeq[E, D, V]
  ): TreapSegmentSeq[E, D, V] =
    segment match {
      case s: NonuniformTreapSegmentBase[_, _, _] => s.patch(otherSeq)
      case _: UniformSegment[_, _, _] => TreapOrderedMap.getFactory.convertMap(otherSeq)
    }
}
