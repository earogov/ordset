package ordset.tree.core

import ordset.Show

object BinaryTreeVisit {

  opaque type Type = Int

  val Left: Type = 0x00000001
  val Right: Type = 0x00000010
  val Both: Type = 0x00000011
  val None: Type = 0x00000000

  private val LeftMask: Int = 0x00000001
  private val RightMask: Int = 0x00000010

  def isLeftVisited(v: Type): Boolean = (v & LeftMask) != 0
  def isRightVisited(v: Type): Boolean = (v & RightMask) != 0

  def isLeftUnvisited(v: Type): Boolean = (v & LeftMask) == 0
  def isRightUnvisited(v: Type): Boolean = (v & RightMask) == 0

  def addLeftVisit(v: Type): Type = v | LeftMask
  def addRightVisit(v: Type): Type = v | RightMask

  def toString(v: Type): String = v match {
    case Left => "Left"
    case Right => "Right"
    case Both => "Both"
    case None => "None"
  }

  implicit val visitShow: Show[BinaryTreeVisit.Type] = Show.show(toString)
}