package ordset.core.segmentSeq

import ordset.Show
import ordset.core.{Bound, ExtendedBound}
import ordset.core.domain.Domain
import ordset.core.interval.{Interval, IntervalRelation}

object SetBuilderFormat { format =>

  // Common definitions ------------------------------------------------------- //
  val indent: String = "  "

  val variable: String = "x"

  val setBegin: String = "{"

  val setEnd: String = "}"

  val emptySet: String = setBegin + setEnd

  val relationSeparator: String = "->"

  val segmentSeparator: String = ","

  val plusInfinity = "+inf"
  
  val minusInfinity = "-inf"

  val belowAllBound: String = s"$variable ${greaterSign(false)} $minusInfinity"

  val aboveAllBound: String = s"$variable ${lessSign(false)} $plusInfinity"

  def lessSign(isIncluding: Boolean): String = if (isIncluding) "<=" else "<"

  def greaterSign(isIncluding: Boolean): String = if (isIncluding) ">=" else ">"

  // Bounds ------------------------------------------------------------------- //
  def upperBound[E](
    bound: Bound.Upper[? <: E],
    elementToStr: E => String = toStringFunc[E]
  ): String =
    s"$variable ${lessSign(bound.isIncluding)} ${elementToStr(bound.element)}"

  def lowerBound[E](
    bound: Bound.Lower[? <: E],
    elementToStr: E => String = toStringFunc[E]
  ): String =
    s"$variable ${greaterSign(bound.isIncluding)} ${elementToStr(bound.element)}"

  def bound[E](
    bound: Bound[? <: E],
    elementToStr: E => String = toStringFunc[E]
  ): String = bound match {
    case b: Bound.Upper[? <: E] => format.upperBound(b, elementToStr)
    case b: Bound.Lower[? <: E] => format.lowerBound(b, elementToStr)
  }

  def lowerExtendedBound[E](
    bound: ExtendedBound.Lower[? <: E],
    elementToStr: E => String = toStringFunc[E]
  ): String = bound match {
    case b: Bound.Lower[? <: E] => format.lowerBound(b, elementToStr)
    case ExtendedBound.BelowAll => belowAllBound
  }

  def upperExtendedBound[E](
    bound: ExtendedBound.Upper[? <: E],
    elementToStr: E => String = toStringFunc[E]
  ): String = bound match {
    case b: Bound.Upper[? <: E] => format.upperBound(b, elementToStr)
    case ExtendedBound.AboveAll => aboveAllBound
  }

  def extendedBound[E](
    bound: ExtendedBound[? <: E],
    elementToStr: E => String = toStringFunc[E]
  ): String = bound match {
    case b: ExtendedBound.Upper[? <: E] => format.upperExtendedBound(b, elementToStr)
    case b: ExtendedBound.Lower[? <: E] => format.lowerExtendedBound(b, elementToStr)
  }

  def boundShow[E](elementShow: Show[? >: E]): Show[Bound[E]] =
    Show.show(b => format.bound(b, elementShow.show))

  def extendedBoundShow[E](elementShow: Show[? >: E]): Show[ExtendedBound[E]] =
    Show.show(b => format.extendedBound(b, elementShow.show))

  // Intervals ---------------------------------------------------------------- //
  val emptyInterval: String = emptySet

  val unboundedInterval: String = s"$setBegin$variable$setEnd"

  def lowerBoundedInterval[E, D[X] <: Domain[X]](
    interval: Interval.Greater[E, D],
    elementToStr: E => String = toStringFunc[E]
  ): String =
    s"$setBegin${format.lowerBound(interval.lower, elementToStr)}$setEnd"

  def upperBoundedInterval[E, D[X] <: Domain[X]](
    interval: Interval.Less[E, D],
    elementToStr: E => String = toStringFunc[E]
  ): String =
    s"$setBegin${format.upperBound(interval.upper, elementToStr)}$setEnd"

  def boundedInterval[E, D[X] <: Domain[X]](
    interval: Interval.Between[E, D],
    elementToStr: E => String = toStringFunc[E]
  ): String = {
    val lowerBound = interval.lower
    val upperBound = interval.upper
    s"$setBegin${elementToStr(lowerBound.element)} ${lessSign(lowerBound.isIncluding)} " +
      s"$variable ${lessSign(upperBound.isIncluding)} ${elementToStr(upperBound.element)}$setEnd"
  }

  def interval[E, D[X] <: Domain[X]](
    interval: Interval[E, D],
    elementToStr: E => String = toStringFunc[E]
  ): String = interval match {
    case _: Interval.Empty[e, d] => format.emptyInterval
    case _: Interval.Unbounded[e, d] => format.unboundedInterval
    case i: Interval.Greater[e, d] => format.lowerBoundedInterval(i, elementToStr)
    case i: Interval.Less[e, d] => format.upperBoundedInterval(i, elementToStr)
    case i: Interval.Between[e, d] => format.boundedInterval(i, elementToStr)
  }

  def intervalShow[E, D[X] <: Domain[X]](elementShow: Show[? >: E]): Show[Interval[E, D]] =
    Show.show(i => format.interval(i, elementShow.show))

  // Interval relations ------------------------------------------------------- //
  def emptyIntervalRelation[E, D[X] <: Domain[X], V](
    value: V,
    valueToStr: V => String = toStringFunc[V]
  ): String =
    s"${format.emptyInterval} $relationSeparator ${valueToStr(value)}"

  def universalIntervalRelation[E, D[X] <: Domain[X], V](
    value: V,
    valueToStr: V => String = toStringFunc[V]
  ): String =
    s"${format.unboundedInterval} $relationSeparator ${valueToStr(value)}"

  def lowerBoundedIntervalRelation[E, D[X] <: Domain[X], V](
    interval: Interval.Greater[E, D],
    value: V,
    elementToStr: E => String = toStringFunc[E],
    valueToStr: V => String = toStringFunc[V]
  ): String =
    s"${format.lowerBoundedInterval(interval, elementToStr)} $relationSeparator ${valueToStr(value)}"

  def upperBoundedIntervalRelation[E, D[X] <: Domain[X], V](
    interval: Interval.Less[E, D],
    value: V,
    elementToStr: E => String = toStringFunc[E],
    valueToStr: V => String = toStringFunc[V]
  ): String =
    s"${format.upperBoundedInterval(interval, elementToStr)} $relationSeparator ${valueToStr(value)}"

  def boundedIntervalRelation[E, D[X] <: Domain[X], V](
    interval: Interval.Between[E, D],
    value: V,
    elementToStr: E => String = toStringFunc[E],
    valueToStr: V => String = toStringFunc[V]
  ): String =
    s"${format.boundedInterval(interval, elementToStr)} $relationSeparator ${valueToStr(value)}"

  def intervalRelation[E, D[X] <: Domain[X], V](
    interval: Interval[E, D],
    value: V,
    elementToStr: E => String = toStringFunc[E],
    valueToStr: V => String = toStringFunc[V]
  ): String =
    s"${format.interval(interval, elementToStr)} $relationSeparator ${valueToStr(value)}"

  def intervalRelationShow[E, D[X] <: Domain[X], V](
    elementShow: Show[? >: E],
    valueShow: Show[V]
  ): Show[IntervalRelation[E, D, V]] =
    Show.show(r => format.intervalRelation(r.interval, r.value, elementShow.show, valueShow.show))

  // Segments ----------------------------------------------------------------- //
  def singleSegment[E, D[X] <: Domain[X], V](
    segment: Segment.Single[E, D, ? <: V],
    valueToStr: V => String = toStringFunc[V]
  ): String = {
    val rel = s"${format.unboundedInterval} $relationSeparator ${valueToStr(segment.value)}"
    s"Segment.Single($rel)"
  }

  def initialSegment[E, D[X] <: Domain[X], V](
    segment: Segment.Initial[E, D, ? <: V],
    elementToStr: E => String = toStringFunc[E],
    valueToStr: V => String = toStringFunc[V]
  ): String = {
    val int = s"$setBegin${format.upperBound(segment.upper, elementToStr)}$setEnd"
    val rel = s"$int $relationSeparator ${valueToStr(segment.value)}"
    s"Segment.Initial($rel)"
  }

  def terminalSegment[E, D[X] <: Domain[X], V](
    segment: Segment.Terminal[E, D, ? <: V],
    elementToStr: E => String = toStringFunc[E],
    valueToStr: V => String = toStringFunc[V]
  ): String = {
    val int = s"$setBegin${format.lowerBound(segment.lower, elementToStr)}$setEnd"
    val rel = s"$int $relationSeparator ${valueToStr(segment.value)}"
    s"Segment.Terminal($rel)"
  }

  def innerSegment[E, D[X] <: Domain[X], V](
    segment: Segment.Inner[E, D, ? <: V],
    elementToStr: E => String = toStringFunc[E],
    valueToStr: V => String = toStringFunc[V]
  ): String = {
    val lowerBound = segment.lower
    val upperBound = segment.upper

    val int = 
      s"$setBegin${elementToStr(lowerBound.element)} ${lessSign(lowerBound.isIncluding)} " +
      s"$variable ${lessSign(upperBound.isIncluding)} ${elementToStr(upperBound.element)}$setEnd"

    val rel = s"$int $relationSeparator ${valueToStr(segment.value)}"
    s"Segment.Inner($rel)"
  }

  def segment[E, D[X] <: Domain[X], V](
    segment: Segment[E, D, ? <: V],
    elementToStr: E => String = toStringFunc[E],
    valueToStr: V => String = toStringFunc[V]
  ): String =
    segment match {
      case s: Segment.Single[E, D, ? <: V] => format.singleSegment(s, valueToStr)
      case s: Segment.Initial[E, D, ? <: V] => format.initialSegment(s, elementToStr, valueToStr)
      case s: Segment.Terminal[E, D, ? <: V] => format.terminalSegment(s, elementToStr, valueToStr)
      case s: Segment.Inner[E, D, ? <: V] => format.innerSegment(s, elementToStr, valueToStr)
      case _ => s"Segment(${format.intervalRelation(segment.interval, segment.value, elementToStr, valueToStr)})"
    }

  def segmentShow[E, D[X] <: Domain[X], V](
    elementShow: Show[? >: E],
    valueShow: Show[? >: V]
  ): Show[Segment[E, D, V]] =
    Show.show(s => format.segment(s, elementShow.show, valueShow.show))

  // Segment sequence --------------------------------------------------------- //
  def segmentSeq[E, D[X] <: Domain[X], V](
    segmentSeq: SegmentSeq[E, D, ? <: V],
    elementToStr: E => String = toStringFunc[E],
    valueToStr: V => String = toStringFunc[V]
  ): String =
    segmentIterable(
      segmentSeq.segments,
      elementToStr,
      valueToStr
    )

  def segmentIterable[E, D[X] <: Domain[X], V](
    segmentIterable: Iterable[Segment[E, D, ? <: V]],
    elementToStr: E => String = toStringFunc[E],
    valueToStr: V => String = toStringFunc[V]
  ): String = {
    val b = new StringBuilder()
    var addSeparator = false
    b.append(System.lineSeparator)
    b.append(setBegin)
    b.append(System.lineSeparator)
    segmentIterable.foreach { s =>
      if (addSeparator) {
        b.append(segmentSeparator)
        b.append(System.lineSeparator)
      }
      val rel = s.intervalRelation
      b.append(indent)
      b.append(format.intervalRelation(rel.interval, rel.value, elementToStr, valueToStr))
      addSeparator = true
    }
    b.append(System.lineSeparator)
    b.append(setEnd)
    b.result()
  }

  def segmentSeqShow[E, D[X] <: Domain[X], V](
    elementShow: Show[E],
    valueShow: Show[V]
  ): Show[SegmentSeq[E, D, V]] =
    Show.show(s => format.segmentSeq(s, elementShow.show, valueShow.show))

  def segmentIterableShow[E, D[X] <: Domain[X], V](
    elementShow: Show[E],
    valueShow: Show[V]
  ): Show[Iterable[Segment[E, D, V]]] =
    Show.show(s => format.segmentIterable(s, elementShow.show, valueShow.show))

  // Utilities ---------------------------------------------------------------- //
  def toStringFunc[E](e: E): String = e.toString
}
