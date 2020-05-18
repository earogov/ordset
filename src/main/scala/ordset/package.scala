package object ordset {

  type Order[A] = cats.kernel.Order[A]
  type Eq[A] = cats.kernel.Eq[A]
  type Hash[A] = cats.kernel.Hash[A]
  type Discrete[A] = cats.collections.Discrete[A]

  val IntAscOrder: Order[Int] = cats.kernel.instances.int.catsKernelStdOrderForInt

  type SetSegment[E] = Segment[E, Boolean]
  type SetSegmentWithPrev[E] = Segment.WithPrev[E, Boolean]
  type SetSegmentWithNext[E] = Segment.WithNext[E, Boolean]
  type SetSegmentInner[E] = Segment.Inner[E, Boolean]
  type SetSegmentSingle[E] = Segment.Single[E, Boolean]
  type SetIntervalMapping[E] = IntervalMapping[E, Boolean]
  type SetSegmentSeq[E] = SegmentSeq[E, Boolean]
}
