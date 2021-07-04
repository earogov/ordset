package ordset.core.internal

import ordset.core.{Bound, SegmentT}
import ordset.core.domain.Domain

object MappedSegmentT {

  /**
   * Segment which has next segment. May be [[Initial]] or [[Inner]].
   *
   * @see [[Segment]]
   */
  trait WithNext[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.WithNext[E, D, V, S2]
      with MappedSegmentLikeT[E, D, U, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def upperBound: Bound.Upper[E] = original.upperBound

    // Navigation --------------------------------------------------------------- //
    override def moveNext: SegmentT.WithPrev[E, D, V, S2] with S2 = consWithPrev(original.moveNext)

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.WithNext[E, D, U, S1]
  }

  /**
   * Segment which has previous segment. May be [[Terminal]] or [[Inner]].
   *
   * @see [[Segment]]
   */
  trait WithPrev[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.WithPrev[E, D, V, S2]
      with MappedSegmentLikeT[E, D, U, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def lowerBound: Bound.Lower[E] = original.lowerBound

    // Navigation --------------------------------------------------------------- //
    override def movePrev: SegmentT.WithNext[E, D, V, S2] with S2 = consWithNext(original.movePrev)

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.WithPrev[E, D, U, S1]
  }


  /**
   * First segment of sequence. Doesn't have previous segment. May be [[Single]] or [[Initial]].
   *
   * @see [[Segment]]
   */
  trait First[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.First[E, D, V, S2]
      with MappedSegmentLikeT[E, D, U, V, S1, S2] {

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.First[E, D, U, S1]
  }

  /**
   * Last segment of sequence. Doesn't have next segment. May be [[Single]] or [[Terminal]].
   *
   * @see [[Segment]]
   */
  trait Last[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.Last[E, D, V, S2]
      with MappedSegmentLikeT[E, D, U, V, S1, S2] {

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.Last[E, D, U, S1]
  }

  /**
   * The only segment in sequence (in case of empty or universal sequence) with properties:
   * <tr>- doesn't have next segment;     </tr>
   * <tr>- doesn't have previous segment. </tr>
   * <tr>                                 </tr>
   * @see [[Segment]]
   */
  trait Single[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.Single[E, D, V, S2]
      with MappedSegmentT.First[E, D, U, V, S1, S2]
      with MappedSegmentT.Last[E, D, U, V, S1, S2] {

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.Single[E, D, U, S1]
  }

  /**
   * Initial segment of sequence with properties:
   * <tr>- doesn't have previous segment; </tr>
   * <tr>- always has next segment.       </tr>
   * <tr>                                 </tr>
   * @see [[Segment]]
   */
  trait Initial[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.Initial[E, D, V, S2]
      with MappedSegmentT.WithNext[E, D, U, V, S1, S2]
      with MappedSegmentT.First[E, D, U, V, S1, S2] {

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.Initial[E, D, U, S1]
  }

  /**
   * Terminal segment of sequence with properties:
   * <tr>- doesn't have next segment;   </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[Segment]]
   */
  trait Terminal[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.Terminal[E, D, V, S2]
      with MappedSegmentT.WithPrev[E, D, U, V, S1, S2]
      with MappedSegmentT.Last[E, D, U, V, S1, S2] {

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.Terminal[E, D, U, S1]
  }

  /**
   * Segment with properties:
   * <tr>- always has next segment;     </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[Segment]]
   */
  trait Inner[E, D <: Domain[E], U, V, S1, +S2]
    extends SegmentT.Inner[E, D, V, S2]
      with MappedSegmentT.WithNext[E, D, U, V, S1, S2]
      with MappedSegmentT.WithPrev[E, D, U, V, S1, S2] {

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.Inner[E, D, U, S1]
  }
}
