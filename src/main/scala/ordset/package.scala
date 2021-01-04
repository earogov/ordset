import ordset.domain.{DirectedOrder, Domain, OrderDir}
import ordset.util.label.Label

package object ordset {

  object OrderLabels {

    val BoundDefault: Label = Label("BoundDefault")

    val SegmentByUpperBound: Label = Label("SegmentByUpperBound")

    val SegmentByLowerBound: Label = Label("SegmentByLowerBound")
  }

  type Eq[A] = cats.kernel.Eq[A]

  type Hash[A] = cats.kernel.Hash[A]

  type Order[A] = cats.kernel.Order[A]

  type Discrete[A] = cats.collections.Discrete[A]

  type Show[A] = cats.Show[A]

  val Show: cats.Show.type = cats.Show

  val Hash: cats.Hash.type = cats.Hash

  val Order: cats.Order.type = cats.Order

  type SegmentOrder[E, D <: Domain[E]] = Order[Segment[E, D, Any]]

  type SegmentOrderWithDir[E, D <: Domain[E],  +Dir <: OrderDir] = DirectedOrder[Segment[E, D, Any], Dir]

  type SetSegment[E, D <: Domain[E]] = Segment[E, D, Boolean]

  type SetSegmentWithPrev[E, D <: Domain[E]] = Segment.WithPrev[E, D, Boolean]

  type SetSegmentWithNext[E, D <: Domain[E]] = Segment.WithNext[E, D, Boolean]

  type SetSegmentInner[E, D <: Domain[E]] = Segment.Inner[E, D, Boolean]

  type SetSegmentSingle[E, D <: Domain[E]] = Segment.Single[E, D, Boolean]

  type SetSegmentSeq[E, D <: Domain[E]] = SegmentSeq[E, D, Boolean]

  type SetIntervalRelation[E, D <: Domain[E]] = IntervalRelation[E, D, Boolean]
}
