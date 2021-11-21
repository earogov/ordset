package ordset

import ordset.{Hash, Show}
import ordset.util.types.{>|<, Dual, Tag}

/** Direction of elements ordering. */
object Direction { 

  /** 
   * Direction base type.
   * 
   * It isn't intended for direct use. The more precise type is [[Type]], which should be used instead.
   */
  opaque type BaseType = Boolean

  /** 
   * Direction type.
   */
  type Type = Asc.Type | Desc.Type

  /** Returns type of opposite direction. */
  type Reversed[Dir <: Type] <: Type = Dir match 
    case Asc.Type => Desc.Type
    case Desc.Type => Asc.Type

  /** Returns opposite direction. */
  def reverse[Dir <: Type](dir: Dir): Reversed[Dir] = (!dir).asInstanceOf

  extension (dir: Type) {

    /** Returns `true` if direction corresponds to ascending order of elements. */
    def isAscending: Boolean = dir

    /** Returns `true` if direction corresponds to descending order of elements. */
    def isDescending: Boolean = !dir

    /** Returns string representation of direction. */
    def asString: String = if dir then "Ascending" else "Descending"
  }

  /** Direction that corresponds to ascending order of elements. */
  object Asc extends Tag.Base[BaseType] {

    /** Value that represents direction at runtime. */
    val value: Asc.Type = fromRaw(true)

    /** Allows to get runtime representation of direction for given type of direction. */
    implicit val singleValue: ValueOf[Asc.Type] = ValueOf(value)

    /** Evidence that direction types are opposite. */
    implicit val dualType: Asc.Type >|< Desc.Type = new Dual[Asc.Type] { override type Out = Desc.Type }
  }

  /** Direction that corresponds to descending order of elements. */
  object Desc extends Tag.Base[BaseType] {

    /** Value that represents direction at runtime. */
    val value: Desc.Type = fromRaw(false)

    /** Allows to get runtime representation of direction for given type of direction. */
    implicit val singleValue: ValueOf[Desc.Type] = ValueOf(value)

    /** Evidence that direction types are opposite. */
    implicit val dualType: Desc.Type >|< Asc.Type = new Dual[Desc.Type] { override type Out = Asc.Type }
  }

  /** Default hash instance for direction. */
  implicit val defaultHash: Hash[Type] = Hash.fromUniversalHashCode

  /** Default show instance for direction. */
  implicit val defaultShow: Show[Type] = (dir: Type) => dir.asString
}