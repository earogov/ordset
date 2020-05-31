package ordset

trait Domain[E] {

  implicit val elementOrd: AscOrder[E]

  implicit val boundOrd: AscOrder[Bound[E]] = Bound.defaultAscOrder

  implicit val segmentOrd: Segment.UpperBoundAscOrder[E] = Segment.upperBoundAscOrder
}
