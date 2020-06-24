package object ordset {

  type OrderDir = OrderDirection.Type
  type AscDir = OrderDirection.Asc.Type
  type DescDir = OrderDirection.Desc.Type

  val AscDir: AscDir = OrderDirection.Asc.Value
  val DescDir: DescDir = OrderDirection.Desc.Value

  type Order[A] = cats.kernel.Order[A]
  type AscOrder[E] = OrderWithDir[E, AscDir]
  type DescOrder[E] = OrderWithDir[E, DescDir]

  type SegmentOrder[E, D <: Domain[E]] = Order[Segment[E, D, Any]]
  type SegmentOrderWithDir[E, D <: Domain[E],  Dir <: OrderDir] = OrderWithDir[Segment[E, D, Any], Dir]

  type Eq[A] = cats.kernel.Eq[A]
  type Hash[A] = cats.kernel.Hash[A]
  type Discrete[A] = cats.collections.Discrete[A]

  val booleanEq: Eq[Boolean] = cats.kernel.instances.boolean.catsKernelStdOrderForBoolean

  type SetSegment[E, D <: Domain[E]] = Segment[E, D, Boolean]
  type SetSegmentWithPrev[E, D <: Domain[E]] = Segment.WithPrev[E, D, Boolean]
  type SetSegmentWithNext[E, D <: Domain[E]] = Segment.WithNext[E, D, Boolean]
  type SetSegmentInner[E, D <: Domain[E]] = Segment.Inner[E, D, Boolean]
  type SetSegmentSingle[E, D <: Domain[E]] = Segment.Single[E, D, Boolean]
  type SetSegmentSeq[E, D <: Domain[E]] = SegmentSeq[E, D, Boolean]
  type SetIntervalMapping[E] = IntervalMapping[E, Boolean]

  type BinaryChoice = BinaryChoice.Type
  type FirstOfTwo = BinaryChoice.First.Type
  type SecondOfTwo = BinaryChoice.Second.Type

  val FistOfTwo: FirstOfTwo = BinaryChoice.First.Value
  val SecondOfTwo: SecondOfTwo = BinaryChoice.Second.Value
}
