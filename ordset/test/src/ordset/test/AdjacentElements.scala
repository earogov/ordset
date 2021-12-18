package ordset.test

case class AdjacentElements[E](predecessor: E | Null, successor: E | Null) {

  def reversed: AdjacentElements[E] = new AdjacentElements(successor, predecessor)
}

object AdjacentElements {

  implicit def fromTuple[E](t: (E | Null, E | Null)): AdjacentElements[E] = new AdjacentElements(t._1, t._2)
}