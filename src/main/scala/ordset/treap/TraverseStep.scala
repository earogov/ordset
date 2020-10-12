package ordset.treap

import ordset.Show
import ordset.util.tag.TaggedRaw

object TraverseStep extends TaggedRaw[Int] { outer =>

  val None: Type = fromRaw(-2)
  val Up: Type = fromRaw(-1)
  val Left: Type = fromRaw(0)
  val Right: Type = fromRaw(1)

  def toString(r: Type): String = r match {
    case None => "None"
    case Up => "Up"
    case Left => "Left"
    case Right => "Right"
  }

  implicit val stepShow: Show[TraverseStep.Type] = Show.show(toString)
}
