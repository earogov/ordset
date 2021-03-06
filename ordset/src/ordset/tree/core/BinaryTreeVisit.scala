package ordset.tree.core

import ordset.Show
import ordset.util.tag.TaggedRaw

object BinaryTreeVisit extends TaggedRaw[Int] {

  val Left: Type = fromRaw(0x00000001)
  val Right: Type = fromRaw(0x00000010)
  val Both: Type = fromRaw(0x00000011)
  val None: Type = fromRaw(0x00000000)

  private val LeftMask: Int = 0x00000001
  private val RightMask: Int = 0x00000010

  def isLeftVisited(v: Type): Boolean = (v & LeftMask) != 0
  def isRightVisited(v: Type): Boolean = (v & RightMask) != 0

  def isLeftUnvisited(v: Type): Boolean = (v & LeftMask) == 0
  def isRightUnvisited(v: Type): Boolean = (v & RightMask) == 0

  def addLeftVisit(v: Type): Type = fromRaw(v | LeftMask)
  def addRightVisit(v: Type): Type = fromRaw(v | RightMask)

  def toString(v: Type): String = v match {
    case Left => "Left"
    case Right => "Right"
    case Both => "Both"
    case None => "None"
  }

  implicit val visitShow: Show[BinaryTreeVisit.Type] = Show.show(toString)
}