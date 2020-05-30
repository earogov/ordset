package ordset

trait OrderedSegmentTuple2[E, +V, +B <: Segment[E, V], +F <: Segment[E, V]] extends SegmentTuple2[E, V, B, F] {

  def domain: Domain[E]
}

object OrderedSegmentTuple2 {

  type UpperBoundAscGen[E, V] = UpperBoundAsc[E, V, Segment[E, V], Segment[E, V]]

  type UpperBoundAscWithNext[E, V] = UpperBoundAsc[E, V, Segment.WithNext[E, V], Segment[E, V]]
  type UpperBoundAscAllWithNext[E, V] = UpperBoundAsc[E, V, Segment.WithNext[E, V], Segment.WithNext[E, V]]

  type UpperBoundAscWithPrev[E, V] = UpperBoundAsc[E, V, Segment[E, V], Segment.WithPrev[E, V]]
  type UpperBoundAscAllWithPrev[E, V] = UpperBoundAsc[E, V, Segment.WithPrev[E, V], Segment.WithPrev[E, V]]

  def upperBoundAsc[E, V](first: Segment.WithNext[E, V], second: Segment.WithNext[E, V])(implicit domain: Domain[E]
  ): UpperBoundAscAllWithNext[E, V] =

    if (domain.boundOrd.compare(first.upperBound, second.upperBound) <= 0) UpperBoundAsc(first, second)
    else                                                                   UpperBoundAsc(second, first)

  def upperBoundAsc[E, V](first: Segment.WithNext[E, V], second: Segment[E, V])(implicit domain: Domain[E]
  ): UpperBoundAscWithNext[E, V] = second match {

    case n: Segment.WithNext[E, V] => upperBoundAsc(first, n)
    case _ =>                         UpperBoundAsc(first, second)
  }

  def upperBoundAsc[E, V](first: Segment[E, V], second: Segment.WithNext[E, V])(implicit domain: Domain[E]
  ): UpperBoundAscWithNext[E, V] = first match {

    case n: Segment.WithNext[E, V] => upperBoundAsc(n, second)
    case _ =>                         UpperBoundAsc(second, first)
  }

  def upperBoundAsc[E, V](first: Segment[E, V], second: Segment[E, V])(implicit domain: Domain[E]
  ): UpperBoundAscGen[E, V] = (first, second) match {

    case (fn: Segment.WithNext[E, V], sn: Segment.WithNext[E, V]) => upperBoundAsc(fn, sn)
    case (fn: Segment.WithNext[E, V], _)                          => UpperBoundAsc(fn, second)
    case (_, sn: Segment.WithNext[E, V])                          => UpperBoundAsc(sn, first)
    case _                                                        => UpperBoundAsc(first, second)
  }

  case class UpperBoundAsc[E, V, +B <: Segment[E, V], +F <: Segment[E, V]](
      override val _1: B,
      override val _2: F
  )(
      implicit override val domain: Domain[E]
  ) extends OrderedSegmentTuple2[E, V, B, F] {

    def backward: B = _1

    def forward: F = _2

//    def stepNext(implicit ev: B <                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        :< Segment.WithNext[E, V]): UpperBoundAscWithPrev[E, V] = _2 match {
//      // X - domain bounds;
//      // | - segment bounds;
//      // ? - unknown bounds (domain or segment).
//      // forward:  ?---------------|
//      // backward: ?---------------|
//      case fn: Segment.WithNext[E, W] if domain.boundOrd.eqv(fn.upperBound, tuple.backward.upperBound) =>
//        makeOrdSegTuple(fn.moveNext, tuple.backward.moveNext)
//      // forward:  ?---------------|
//      // backward: ?----------|
//      // Value of forward segment is invariant =>
//      // we can right away define operator value up to `forward.upperBound`
//      // skipping segments of backward sequence below it.
//      case fn: Segment.WithNext[E, W] if invariant(fn.value) =>
//        makeOrdSegTuple(fn, tuple.backward.moveTo(fn.upperBound))
//      // forward:  ?---------------X
//      // backward: ?----------|
//      // Value of forward segment is invariant
//      case f if invariant(f.value) =>
//        makeOrdSegTuple(f, tuple.backward.moveToLast)
//      // forward:  ?---------------?
//      // backward: ?----------|
//      // Value of forward segment is not invariant
//      case f =>
//        makeOrdSegTuple(f, tuple.backward.moveNext)
//    }
  }

  // TODO: add UpperBoundDesc cases and maybe LowerBoundAsc/Desc.
}
