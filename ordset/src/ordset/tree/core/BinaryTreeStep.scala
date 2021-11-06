package ordset.tree.core

import ordset.Show

object BinaryTreeStep {

  opaque type Type = Int

  val None: Type = -2
  val Up: Type = -1
  val Left: Type = 0
  val Right: Type = 1

  def toString(r: Type): String = r match {
    case None => "None"
    case Up => "Up"
    case Left => "Left"
    case Right => "Right"
  }

  implicit val stepShow: Show[BinaryTreeStep.Type] = Show.show(toString)
}
