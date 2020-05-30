package ordset

import ordset.util.SingleValue
import tag._

object OrderDirection extends TaggedRaw[Boolean] { outer =>

  // Copy of `Type` to avoid cyclic aliasing error in `Asc` and `Desc`.
  type Alias = Boolean with Tag[this.type]

  def flip(dir: Type): Type = fromRaw(!dir)
  def isAscending(dir: Type): Boolean = dir
  def isDescending(dir: Type): Boolean = !dir

  object Asc extends TaggedBase[Boolean, outer.Alias] {

    val Value: Type = fromRaw(true)

    def flip(dir: Type): outer.Desc.Type = outer.Desc.Value

    implicit def singleValue: SingleValue[Type] = new SingleValue[Type] {
      override def get: Type = Value
    }
  }

  object Desc extends TaggedBase[Boolean, outer.Alias] {

    val Value: Type = fromRaw(false)

    def flip(dir: Type): outer.Asc.Type = outer.Asc.Value

    implicit def singleValue: SingleValue[Type] = new SingleValue[Type] {
      override def get: Type = Value
    }
  }
}