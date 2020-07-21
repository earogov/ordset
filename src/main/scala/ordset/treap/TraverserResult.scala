package ordset.treap

import ordset.tag.TaggedRaw

object TraverserResult extends TaggedRaw[Int] {

  val Stop: Type = fromRaw(0)
  val Left: Type = fromRaw(1)
  val Right: Type = fromRaw(2)
  val Up: Type = fromRaw(3)

  def toString(r: Type): String = r match {
    case Stop => "Stop"
    case Left => "Left"
    case Right => "Right"
    case Up => "Up"
  }
}