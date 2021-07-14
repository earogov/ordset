//package ordset.core
//
//import ordset.core.domain.Domain
//import AbstractMappedSegmentSeq._
//
///**
// * {{{
// *         A              B                C
// * X-------------](--------------)[---------------X
// *         |              |               |
// *         V              V               V
// *
// *         A              A               D
// * X-----------------------------)[---------------X
// * }}}
// */
//abstract class AbstractMappedSegmentSeq[E, D <: Domain[E], U, V, S]
//  extends AbstractSegmentSeq[E, D, AbstractMappedSegmentSeq.MappedSegment[E, D, U, V, S]] {
//
//  // Inspection --------------------------------------------------------------- //
//  /** Original sequence to which mapping is applied. */
//  val originalSeq: SegmentSeqT[E, D, U, S]
//
//  /** Mapping function for segment value. */
//  val mapFunc: U => V
//
//  final override def isEmpty: Boolean =
//    firstSegmentInstance.isSingle && !isValueIncluded(firstSegmentInstance.value)
//
//  final override def isUniversal: Boolean =
//    firstSegmentInstance.isSingle && isValueIncluded(firstSegmentInstance.value)
//
//  final override def isUniform: Boolean =
//    firstSegmentInstance.isSingle
//
//  final override def contains(bound: Bound[E]): Boolean =
//    isValueIncluded(getSegmentValue(originalSeq.getSegment(bound)))
//
//  final override def containsElement(element: E): Boolean = super.containsElement(element)
//
//  // Navigation --------------------------------------------------------------- //
//  final override def upperBounds: Iterable[Bound.Upper[E]] = super.upperBounds
//
//  final override def firstSegment: MappedFirstSegment[E, D, U, V, S] =
//    firstSegmentInstance
//
//  final override def lastSegment: MappedLastSegment[E, D, U, V, S] =
//    lastFrontMapper(originalSeq.lastSegment)
//
//  final override def getSegment(bound: Bound[E]): MappedSegment[E, D, U, V, S] =
//    searchFrontMapper(generalFrontMapper, originalSeq.getSegment(bound))
//
//  final override def getSegmentForElement(element: E): MappedSegment[E, D, U, V, S] =
//    super.getSegmentForElement(element)
//
//  // Protected section -------------------------------------------------------- //
//  /**
//   * Returns `true` if segment with given value is considered to be included in set.
//   *
//   * For example, if `V` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
//   */
//  protected def isValueIncluded(value: V): Boolean
//
//  /**
//   * First segment of sequence. It's either initial ot single.
//   */
//  protected final lazy val firstSegmentInstance: MappedFirstSegment[E, D, U, V, S] =
//    searchFrontMapper(firstFrontMapper, originalSeq.firstSegment)
//
//  /**
//   * @return value of mapped segment defined by `original` segment.
//   * For sets it returns 'belongs to set' indicator (`Boolean`).
//   * For maps `E` -> `W` it returns `Option[W]` where `None` means that segment doesn't belong to set.
//   */
//  protected final def getSegmentValue(original: SegmentT[E, D, U, S]): V = mapFunc.apply(original.value)
//}
//
//object AbstractMappedSegmentSeq {
//
//  type MappedSegment[E, D <: Domain[E], V, U, S] =
//    SegmentT[E, D, V, MappedSegmentBase[E, D, U, V, S]] with MappedSegmentBase[E, D, U, V, S]
//
//  type MappedFirstSegment[E, D <: Domain[E], V, U, S] =
//    SegmentT[E, D, V, MappedSegmentBase[E, D, U, V, S]] with MappedSegmentBase[E, D, U, V, S]
//
//  type MappedLastSegment[E, D <: Domain[E], V, U, S] =
//    SegmentT[E, D, V, MappedSegmentBase[E, D, U, V, S]] with MappedSegmentBase[E, D, U, V, S]
//
//  /**
//   * Base trait for mapped segments.
//   *
//   * Upper and lower bounds of mapped segments are defined by change of new (mapped) value.
//   * Mapped segment is specified by 'original' segment that corresponds to the upper bound.
//   * {{{
//   *
//   *                                     original
//   *                                        V
//   *       -------------|-------------|-------------|-------------
//   *              A     |     B              C      |     D         - original value
//   *                    |                           |
//   *       prev mapped  |          mapped (this)    |     next mapped
//   *           V        |              V            |         V
//   *       -------------|---------------------------|-------------
//   *             A      |              B            |      C        - mapped value
//   *                lower bound                upper bound
//   * }}}
//   */
//  sealed trait MappedSegmentBase[E, D <: Domain[E], U, V, S]
//    extends SegmentLikeT[E, D, V, MappedSegmentBase[E, D, U, V, S]] {
//
//    // Inspection --------------------------------------------------------------- //
//    /**
//     * Original segment to which mapping is applied.
//     */
//    def original: SegmentT[E, D, U, S]
//
//    /**
//     * @return `true` if this segment maps input `segment`.
//     */
//    def isSpecifiedBy(segment: SegmentT[E, D, U, S]): Boolean =
//      sequence.originalSeq.eq(segment.sequence) && sequence.domainOps.segmentUpperOrd.eqv(original, segment)
//
//    override def sequence: MappedSegmentSeq[E, D, U, V, S]
//
//    override lazy val value: V = sequence.getSegmentValue(this)
//
//    override def isIncluded: Boolean = sequence.isValueIncluded(value)
//
//    // Navigation --------------------------------------------------------------- //
//    override def moveToFirst: MappedFirstSegment[E, D, U, V, S] = sequence.firstSegment
//
//    override def moveToLast: MappedLastSegment[E, D, U, V, S] = sequence.lastSegment
//
//    override def moveTo(bound: Bound[E]): MappedSegment[E, D, U, V, S] =
//      sequence.searchFrontMapper(sequence.generalFrontMapper, original.moveTo(bound))
//
//    // Transformation ----------------------------------------------------------- //
//    override def takeAbove: SegmentSeq[E, D, V] = ???
//
//    override def takeBelow: SegmentSeq[E, D, V] = ???
//
//    override def slice: (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = ???
//
//    override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???
//
//    override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???
//
//    override def truncation(bound: Bound[E]): SegmentLikeT.Truncation[E, D, V, MappedSegmentBase[E, D, U, V, S]] = ???
//
//  }
//
//  /**
//   * Mapped segment with next segment.
//   *
//   * Segment is specified by 'original' segment that corresponds to the upper bound.
//   */
//  sealed trait MappedSegmentWithNext[E, D <: Domain[E], U, V, S]
//    extends SegmentT.WithNext[E, D, V, MappedSegmentBase[E, D, U, V, S]]
//      with MappedSegmentBase[E, D, U, V, S] {
//
//    // Inspection --------------------------------------------------------------- //
//    override def original: SegmentT.WithNext[E, D, U, S]
//
//    override def self: MappedSegmentWithNext[E, D, U, V, S]
//
//    override def upperBound: Bound.Upper[E] = original.upperBound
//
//    // Navigation --------------------------------------------------------------- //
//    override def moveNext: MappedSegmentWithPrev[E, D, U, V, S] = ???
//  }
//
//  /**
//   * Mapped segment with previous segment.
//   *
//   * Segment is specified by 'original' segment that corresponds to the upper bound.
//   *
//   * Lower bound is defined by `back` segment and is searched lazily.
//   * {{{
//   *
//   *                          back        original
//   *                           V             V
//   *       -------------|-------------|-------------|-------------
//   *              A     |     B              C      |     D         - original value
//   *                    |                           |
//   *                    |          mapped (this)    |
//   *                    |              V            |
//   *       -------------|---------------------------|-------------
//   *             A                     B                  C         - mapped value
//   * }}}
//   *
//   * Preconditions:
//   *
//   * 1. 'back' subsegment must have previous segment.
//   *    This condition is equivalent to: mapped segment has previous segment.
//   */
//  sealed trait MappedSegmentWithPrev[E, D <: Domain[E], U, V, S]
//    extends SegmentT.WithPrev[E, D, V, MappedSegmentBase[E, D, U, V, S]]
//      with MappedSegmentBase[E, D, U, V, S] {
//
//    // Inspection --------------------------------------------------------------- //
//    lazy val back: SegmentT.WithPrev[E, D, U, S] = ???
//
//    override def self: MappedSegmentWithPrev[E, D, U, V, S]
//
//    override def lowerBound: Bound.Lower[E] = back.lowerBound
//
//    // Navigation --------------------------------------------------------------- //
//    override def movePrev: MappedSegmentWithNext[E, D, U, S] = ???
//  }
//
//  /**
//   * Initial segment of mapped sequence.
//   *
//   * Segment is specified by 'original' segment that corresponds to the upper bound.
//   */
//  final case class MappedInitialSegment[E, D <: Domain[E], U, V, S](
//    override val sequence: MappedSegmentSeq[E, D, U, V, S],
//    override val original: SegmentT.WithNext[E, D, U, S]
//  ) extends SegmentT.Initial[E, D, V, MappedSegmentBase[E, D, U, V, S]]
//    with MappedSegmentWithNext[E, D, U, V, S] {
//
//    // Inspection --------------------------------------------------------------- //
//    override def self: MappedInitialSegment[E, D, U, V, S] = this
//
//    // Navigation --------------------------------------------------------------- //
//    override def moveToFirst: MappedInitialSegment[E, D, U, V, S] = this
//
//  }
//
//  /**
//   * Terminal segment of mapped sequence.
//   *
//   * Segment is specified by 'original' segment that must be the last segment of original sequence.
//   */
//  final case class MappedTerminalSegment[E, D <: Domain[E], U, V, S](
//    override val sequence: MappedSegmentSeq[E, D, U, V, S],
//    override val original: SegmentT.Last[E, D, U, S]
//  ) extends SegmentT.Terminal[E, D, V, MappedSegmentBase[E, D, U, V, S]]
//    with MappedSegmentWithPrev[E, D, U, V, S] {
//
//    // Inspection --------------------------------------------------------------- //
//    override def isSpecifiedBy(segment: SegmentT[E, D, U, S]): Boolean =
//      sequence.originalSeq.eq(segment.sequence) && segment.isLast
//
//    override def self: MappedTerminalSegment[E, D, U, V, S] = this
//
//    // Navigation --------------------------------------------------------------- //
//    override def moveToLast: MappedTerminalSegment[E, D, U, V, S] = this
//  }
//
//  /**
//   * Inner segment of mapped sequence.
//   *
//   * Segment is specified by 'original' segment that corresponds to the upper bound.
//   */
//  final case class MappedInnerSegment[E, D <: Domain[E], U, V, S](
//    override val sequence: MappedSegmentSeq[E, D, U, V, S],
//    override val original: SegmentT.WithNext[E, D, U, S]
//  ) extends SegmentT.Inner[E, D, V, MappedSegmentBase[E, D, U, V, S]]
//    with MappedSegmentWithPrev[E, D, U, V, S]
//    with MappedSegmentWithNext[E, D, U, V, S] {
//
//    // Inspection --------------------------------------------------------------- //
//    override def self: MappedInnerSegment[E, D, U, V, S] = this
//  }
//
//  /**
//   * Single segment of mapped sequence.
//   *
//   * Segment is specified by 'original' segment that must be the last segment of original sequence.
//   */
//  final case class MappedSingleSegment[E, D <: Domain[E], U, V, S](
//    override val sequence: MappedSegmentSeq[E, D, U, V, S],
//    override val original: SegmentT.Last[E, D, U, S]
//  ) extends SegmentT.Single[E, D, V, MappedSegmentBase[E, D, U, V, S]]
//    with MappedSegmentBase[E, D, U, V, S] {
//
//    // Inspection --------------------------------------------------------------- //
//    override def isSpecifiedBy(segment: SegmentT[E, D, U, S]): Boolean =
//      sequence.originalSeq.eq(segment.sequence) && segment.isLast
//
//    override def self: MappedSingleSegment[E, D, U, V, S] = this
//
//    // Navigation --------------------------------------------------------------- //
//    override def moveToFirst: MappedSingleSegment[E, D, U, V, S] = this
//
//    override def moveToLast: MappedSingleSegment[E, D, U, V, S] = this
//
//    override def moveTo(bound: Bound[E]): MappedSingleSegment[E, D, U, V, S] = this
//  }
//
//  // Protected section -------------------------------------------------------- //
//  protected type Mapper[E, D <: Domain[E], U, V, S, +R <: MappedSegment[E, D, U, V, S]] = SegmentT[E, D, U, S] => R
//}