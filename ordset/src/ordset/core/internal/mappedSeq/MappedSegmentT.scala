package ordset.core.internal.mappedSeq

import ordset.core.{Bound, SegmentT}
import ordset.core.domain.Domain

protected[ordset] object MappedSegmentT {

  /**
   * Segment which has next segment. May be [[Initial]] or [[Inner]].
   *
   * @see [[MappedSegmentLikeT]]
   */
  trait WithNext[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.WithNext[E, D, V, S2]
      with MappedSegmentLikeT[E, D, U, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedSegmentT.WithNext[E, D, U, V, S1, S2] with S2

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.WithNext[E, D, U, S1]
  }

  /**
   * Segment which has previous segment. May be [[Terminal]] or [[Inner]].
   *
   * @see [[MappedSegmentLikeT]]
   */
  trait WithPrev[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.WithPrev[E, D, V, S2]
      with MappedSegmentLikeT[E, D, U, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedSegmentT.WithPrev[E, D, U, V, S1, S2] with S2

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.WithPrev[E, D, U, S1]
  }


  /**
   * First segment of sequence. Doesn't have previous segment. May be [[Single]] or [[Initial]].
   *
   * @see [[MappedSegmentLikeT]]
   */
  trait First[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.First[E, D, V, S2]
      with MappedSegmentLikeT[E, D, U, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedSegmentT.First[E, D, U, V, S1, S2] with S2
  }

  /**
   * Last segment of sequence. Doesn't have next segment. May be [[Single]] or [[Terminal]].
   *
   * @see [[Segment]]
   */
  trait Last[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.Last[E, D, V, S2]
      with MappedSegmentLikeT[E, D, U, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedSegmentT.Last[E, D, U, V, S1, S2] with S2
  }

  /**
   * The only segment in sequence (in case of empty or universal sequence) with properties:
   * <tr>- doesn't have next segment;     </tr>
   * <tr>- doesn't have previous segment. </tr>
   * <tr>                                 </tr>
   * @see [[MappedSegmentLikeT]]
   */
  trait Single[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.Single[E, D, V, S2]
      with MappedSegmentT.First[E, D, U, V, S1, S2]
      with MappedSegmentT.Last[E, D, U, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedSegmentT.Single[E, D, U, V, S1, S2] with S2
  }

  /**
   * Initial segment of sequence with properties:
   * <tr>- doesn't have previous segment; </tr>
   * <tr>- always has next segment.       </tr>
   * <tr>                                 </tr>
   * @see [[MappedSegmentLikeT]]
   */
  trait Initial[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.Initial[E, D, V, S2]
      with MappedSegmentT.WithNext[E, D, U, V, S1, S2]
      with MappedSegmentT.First[E, D, U, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedSegmentT.Initial[E, D, U, V, S1, S2] with S2
  }

  /**
   * Terminal segment of sequence with properties:
   * <tr>- doesn't have next segment;   </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[MappedSegmentLikeT]]
   */
  trait Terminal[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.Terminal[E, D, V, S2]
      with MappedSegmentT.WithPrev[E, D, U, V, S1, S2]
      with MappedSegmentT.Last[E, D, U, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedSegmentT.Terminal[E, D, U, V, S1, S2] with S2
  }

  /**
   * Segment with properties:
   * <tr>- always has next segment;     </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[MappedSegmentLikeT]]
   */
  trait Inner[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.Inner[E, D, V, S2]
      with MappedSegmentT.WithNext[E, D, U, V, S1, S2]
      with MappedSegmentT.WithPrev[E, D, U, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedSegmentT.Inner[E, D, U, V, S1, S2] with S2

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.Inner[E, D, U, S1]
  }
}
