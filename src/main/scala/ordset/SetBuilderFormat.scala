package ordset

import ordset.domain.Domain

object SetBuilderFormat { format =>

  // Common definitions ------------------------------------------------------- //
  val variable: String = "x"

  val setBegin: String = "{"

  val setEnd: String = "}"

  val emptySet: String = setBegin + setEnd

  val universalSet: String = "U"

  val relationSeparator: String = "->"

  def lessSign(isInclusive: Boolean): String = if (isInclusive) "<=" else "<"

  def greaterSign(isInclusive: Boolean): String = if (isInclusive) ">=" else ">"

  // Bounds ------------------------------------------------------------------- //
  def upperBound[E](bound: Bound.Upper[E], elementToStr: E => String): String =
    s"$variable ${lessSign(bound.isInclusive)} ${elementToStr(bound.value)}"

  def lowerBound[E](bound: Bound.Lower[E], elementToStr: E => String): String =
    s"$variable ${greaterSign(bound.isInclusive)} ${elementToStr(bound.value)}"

  def bound[E](bound: Bound[E], elementToStr: E => String): String = bound match {
    case b: Bound.Upper[E] => format.upperBound(b, elementToStr)
    case b: Bound.Lower[E] => format.lowerBound(b, elementToStr)
  }

  def boundShow[E](elementShow: Show[E]): Show[Bound[E]] =
    Show.show(b => format.bound(b, elementShow.show))

  // Intervals ---------------------------------------------------------------- //
  val emptyInterval: String = emptySet

  val universalInterval: String = s"$setBegin$variable in $universalSet$setEnd"

  def lowerBoundedInterval[E, D <: Domain[E]](interval: Interval.Greater[E, D], elementToStr: E => String): String =
    s"$setBegin${format.lowerBound(interval.lowerBound, elementToStr)}$setEnd"

  def upperBoundedInterval[E, D <: Domain[E]](interval: Interval.Less[E, D], elementToStr: E => String): String =
    s"$setBegin${format.upperBound(interval.upperBound, elementToStr)}$setEnd"

  def boundedInterval[E, D <: Domain[E]](interval: Interval.Between[E, D], elementToStr: E => String): String = {
    val lowerBound = interval.lowerBound
    val upperBound = interval.upperBound
    s"$setBegin${elementToStr(lowerBound.value)} ${lessSign(lowerBound.isInclusive)} " +
      s"$variable ${lessSign(upperBound.isInclusive)} ${elementToStr(upperBound.value)}$setEnd"
  }

  def interval[E, D <: Domain[E]](interval: Interval[E, D], elementToStr: E => String): String = interval match {
    case _: Interval.Empty[E, D] => format.emptyInterval
    case _: Interval.Universal[E, D] => format.universalInterval
    case i: Interval.Greater[E, D] => format.lowerBoundedInterval(i, elementToStr)
    case i: Interval.Less[E, D] => format.upperBoundedInterval(i, elementToStr)
    case i: Interval.Between[E, D] => format.boundedInterval(i, elementToStr)
  }

  def intervalShow[E, D <: Domain[E]](elementShow: Show[E]): Show[Interval[E, D]] =
    Show.show(i => format.interval(i, elementShow.show))

  // Interval relations ------------------------------------------------------- //
  def emptyIntervalRelation[E, D <: Domain[E], V](
    value: V,
    valueToStr: V => String
  ): String =
    s"${format.emptyInterval} $relationSeparator ${valueToStr(value)}"

  def universalIntervalRelation[E, D <: Domain[E], V](
    value: V,
    valueToStr: V => String
  ): String =
    s"${format.universalInterval} $relationSeparator ${valueToStr(value)}"

  def lowerBoundedIntervalRelation[E, D <: Domain[E], V](
    interval: Interval.Greater[E, D],
    value: V,
    elementToStr: E => String,
    valueToStr: V => String
  ): String =
    s"${format.lowerBoundedInterval(interval, elementToStr)} $relationSeparator ${valueToStr(value)}"

  def upperBoundedIntervalRelation[E, D <: Domain[E], V](
    interval: Interval.Less[E, D],
    value: V,
    elementToStr: E => String,
    valueToStr: V => String
  ): String =
    s"${format.upperBoundedInterval(interval, elementToStr)} $relationSeparator ${valueToStr(value)}"

  def boundedIntervalRelation[E, D <: Domain[E], V](
    interval: Interval.Between[E, D],
    value: V,
    elementToStr: E => String,
    valueToStr: V => String
  ): String =
    s"${format.boundedInterval(interval, elementToStr)} $relationSeparator ${valueToStr(value)}"

  def intervalRelation[E, D <: Domain[E], V](
    interval: Interval[E, D],
    value: V,
    elementToStr: E => String,
    valueToStr: V => String
  ): String =
    s"${format.interval(interval, elementToStr)} $relationSeparator ${valueToStr(value)}"

  def intervalRelationShow[E, D <: Domain[E], V](
    elementShow: Show[E],
    valueShow: Show[V]
  ): Show[IntervalRelation[E, D, V]] =
    Show.show(r => format.intervalRelation(r.interval, r.value, elementShow.show, valueShow.show))

  // Segments ----------------------------------------------------------------- //
  def singleSegment[E, D <: Domain[E], V](
    segment: Segment.Single[E, D, V],
    valueToStr: V => String
  ): String =
    s"Segment.Single(${format.universalIntervalRelation(segment.value, valueToStr)})"

  def initialSegment[E, D <: Domain[E], V](
    segment: Segment.Initial[E, D, V],
    elementToStr: E => String,
    valueToStr: V => String
  ): String =
    s"Segment.Initial(${format.intervalRelation(segment.interval, segment.value, elementToStr, valueToStr)})"

  def terminalSegment[E, D <: Domain[E], V](
    segment: Segment.Terminal[E, D, V],
    elementToStr: E => String,
    valueToStr: V => String
  ): String =
    s"Segment.Terminal(${format.intervalRelation(segment.interval, segment.value, elementToStr, valueToStr)})"

  def innerSegment[E, D <: Domain[E], V](
    segment: Segment.Inner[E, D, V],
    elementToStr: E => String,
    valueToStr: V => String
  ): String =
    s"Segment.Inner(${format.intervalRelation(segment.interval, segment.value, elementToStr, valueToStr)})"

  def segment[E, D <: Domain[E], V](
    segment: Segment[E, D, V],
    elementToStr: E => String,
    valueToStr: V => String
  ): String =
    segment match {
      case s: Segment.Single[E, D, V] => format.singleSegment(s, valueToStr)
      case s: Segment.Initial[E, D, V] => format.initialSegment(s, elementToStr, valueToStr)
      case s: Segment.Terminal[E, D, V] => format.terminalSegment(s, elementToStr, valueToStr)
      case s: Segment.Inner[E, D, V] => format.innerSegment(s, elementToStr, valueToStr)
      case _ => s"Segment(${format.intervalRelation(segment.interval, segment.value, elementToStr, valueToStr)})"
    }

  def segmentShow[E, D <: Domain[E], V](
    elementShow: Show[E],
    valueShow: Show[V]
  ): Show[Segment[E, D, V]] =
    Show.show(s => format.segment(s, elementShow.show, valueShow.show))
}
