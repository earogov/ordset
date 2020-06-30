package ordset

trait Domain[E] {

  implicit val elementOrd: AscOrder[E]

  implicit val boundOrd: AscOrder[Bound[E]] = Bound.defaultAscOrder

  implicit val segmentUpperOrd: Segment.UpperBoundAscOrder[E] = Segment.upperBoundAscOrder

  implicit val segmentLowerOrd: Segment.LowerBoundAscOrder[E] = Segment.lowerBoundAscOrder
}
