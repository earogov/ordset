package ordset.core.segmentSeq.internal

import ordset.core.segmentSeq.{Segment, SetBuilderFormat}
import ordset.core.domain.Domain

protected[ordset] object SegmentSeqExceptionUtil {

  @throws[NoSuchElementException]
  final def throwNoNextSegment[E, D[X] <: Domain[X]](segment: Segment[E, D, ?]): Nothing = {
    throw new NoSuchElementException(s"Segment $segment doesn't have next segment.")
  }

  @throws[NoSuchElementException]
  final def throwNoPrevSegment[E, D[X] <: Domain[X]](segment: Segment[E, D, ?]): Nothing = {
    throw new NoSuchElementException(s"Segment $segment doesn't have previous segment.")
  }

  @throws[AssertionError]
  final def throwSegmentMustHaveOneOfBaseTypes[E, D[X] <: Domain[X]](segment: Segment[E, D, ?]): Nothing = {
    throw new AssertionError(
      s"Expected that segment $segment has one of base types: inner, initial, terminal or single."
    )
  }

  @throws[AssertionError]
  final def throwSegmentMustBeLastOrWithNext[E, D[X] <: Domain[X]](segment: Segment[E, D, ?]): Nothing = {
    throw new AssertionError(
      s"Expected that segment $segment is either last or has next segment."
    )
  }

  @throws[AssertionError]
  final def throwSegmentMustBeFirstOrWithPrev[E, D[X] <: Domain[X]](segment: Segment[E, D, ?]): Nothing = {
    throw new AssertionError(
      s"Expected that segment $segment is either first or has previous segment."
    )
  }

  @throws[AssertionError]
  final def throwSegmentMustBeInitialOrSingle[E, D[X] <: Domain[X]](segment: Segment[E, D, ?]): Nothing = {
    throw new AssertionError(
      s"Expected that segment $segment is either initial or single."
    )
  }

  @throws[AssertionError]
  final def throwSegmentMustBeTerminalOrSingle[E, D[X] <: Domain[X]](segment: Segment[E, D, ?]): Nothing = {
    throw new AssertionError(
      s"Expected that segment $segment is either terminal or single."
    )
  }

  @throws[AssertionError]
  final def throwSegmentsMustBeLastOrWithNext[E, D[X] <: Domain[X]](segments: Segment[E, D, ?]*): Nothing = {
    val segmentsStr = SetBuilderFormat.segmentIterable(
      segments, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[Any]
    )
    throw new AssertionError(
      s"Expected that segments $segmentsStr are either last or has next segment."
    )
  }

  @throws[AssertionError]
  final def throwSegmentsMustBeFirstOrWithPrev[E, D[X] <: Domain[X]](segments: Segment[E, D, ?]*): Nothing = {
    val segmentsStr = SetBuilderFormat.segmentIterable(
      segments, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[Any]
    )
    throw new AssertionError(
      s"Expected that segments $segmentsStr are either first or has previous segment."
    )
  }
}
