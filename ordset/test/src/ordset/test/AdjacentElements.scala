package ordset.test

import ordset.Discrete

case class AdjacentElements[E](predecessor: Discrete.Maybe[E], successor: Discrete.Maybe[E]) {

  def reversed: AdjacentElements[E] = new AdjacentElements(successor, predecessor)

  def predecessorOpt: Option[E] = predecessor match {
    case Discrete.None => Option.empty
    case p: E @unchecked => Some(p)
  }

  def successorOpt: Option[E] = successor match {
    case Discrete.None => Option.empty
    case s: E @unchecked => Some(s)
  }
}

object AdjacentElements {

  implicit def fromTuple[E](t: (Discrete.Maybe[E], Discrete.Maybe[E])): AdjacentElements[E] = 
    new AdjacentElements(t._1, t._2)
}