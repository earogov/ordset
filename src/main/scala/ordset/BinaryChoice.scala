package ordset

import ordset.tag.{Tag, TaggedBase, TaggedRaw}
import ordset.util.SingleValue

object BinaryChoice extends TaggedRaw[Boolean] { outer =>

  // Copy of `Type` to avoid cyclic aliasing error in `First` and `Second`.
  type Alias = Boolean with Tag[this.type]

  def flip(choice: Type): Type = fromRaw(!choice)
  def isFirst(choice: Type): Boolean = choice
  def isSecond(choice: Type): Boolean = !choice

  object First extends TaggedBase[Boolean, outer.Alias] {

    val Value: Type = fromRaw(true)

    def flip(choice: Type): outer.Second.Type = outer.Second.Value

    implicit def singleValue: SingleValue[Type] = new SingleValue[Type] {
      override def get: Type = Value
    }
  }

  object Second extends TaggedBase[Boolean, outer.Alias] {

    val Value: Type = fromRaw(false)

    def flip(choice: Type): outer.First.Type = outer.First.Value

    implicit def singleValue: SingleValue[Type] = new SingleValue[Type] {
      override def get: Type = Value
    }
  }
}
