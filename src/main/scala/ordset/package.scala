package object ordset {

  type OrderDir = OrderDirection.Type
  type AscDir = OrderDirection.Asc.Type
  type DescDir = OrderDirection.Desc.Type

  val AscDir: AscDir = OrderDirection.Asc.Value
  val DescDir: DescDir = OrderDirection.Desc.Value

  type Order[A] = cats.kernel.Order[A]
  type AscOrder[E] = OrderWithDir[E, AscDir]
  type DescOrder[E] = OrderWithDir[E, DescDir]

  type Eq[A] = cats.kernel.Eq[A]
  type Hash[A] = cats.kernel.Hash[A]
  type Discrete[A] = cats.collections.Discrete[A]

  val booleanEq: Eq[Boolean] = cats.kernel.instances.boolean.catsKernelStdOrderForBoolean

  type SetSegment[E] = Segment[E, Boolean]
  type SetSegmentWithPrev[E] = Segment.WithPrev[E, Boolean]
  type SetSegmentWithNext[E] = Segment.WithNext[E, Boolean]
  type SetSegmentInner[E] = Segment.Inner[E, Boolean]
  type SetSegmentSingle[E] = Segment.Single[E, Boolean]
  type SetIntervalMapping[E] = IntervalMapping[E, Boolean]
  type SetSegmentSeq[E] = SegmentSeq[E, Boolean]
}
