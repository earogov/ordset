package ordset

trait Domain[E] {

  implicit val elementOrd: AscOrder[E]

  implicit val boundOrd: AscOrder[Bound[E]]

  implicit val segmentOrd: AscOrder[Segment[E, Nothing]]
}
