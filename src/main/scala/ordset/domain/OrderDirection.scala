package ordset.domain

import ordset.{Hash, Show}
import ordset.util.tag._
import ordset.util.types.{>|<, Dual, SingleValue}

object OrderDirection extends TaggedRaw[Boolean] { outer =>

  // Copy of `Type` to avoid cyclic aliasing error in `Asc` and `Desc`.
  type Alias = Boolean with Tag[this.type]

  def reverse(dir: Type): Type = fromRaw(!dir)

  def reverseTyped[Dir <: Type, Rev <: Type](dir: Dir)(implicit ev: Dir >|< Rev): Rev = fromRaw(!dir).asInstanceOf[Rev]

  def isAscending(dir: Type): Boolean = dir

  def isDescending(dir: Type): Boolean = !dir

  def toString(dir: Type): String = if (dir) "Ascending" else "Descending"

  implicit val defaultHash: Hash[Type] = Hash.fromUniversalHashCode

  implicit val defaultShow: Show[Type] = toString

  object Asc extends TaggedBase[Boolean, outer.Alias] {

    val value: Asc.Type = fromRaw(true)

    implicit val singleValue: SingleValue[Asc.Type] = new SingleValue[Asc.Type] {
      override def get: Asc.Type = value
    }

    implicit val dualType: Asc.Type >|< Desc.Type = new Dual[Asc.Type] { override type Out = Desc.Type }
  }

  object Desc extends TaggedBase[Boolean, outer.Alias] {

    val value: Desc.Type = fromRaw(false)

    implicit val singleValue: SingleValue[Desc.Type] = new SingleValue[Desc.Type] {
      override def get: Desc.Type = value
    }

    implicit val dualType: Desc.Type >|< Asc.Type = new Dual[Desc.Type] { override type Out = Asc.Type }
  }
}