package ordset.util.types

trait Tag[T] extends Any

object Tag {

  trait Base[Raw] {

    type Type = Raw & Tag[this.type]

    def fromRaw(raw: Raw): Type = raw.asInstanceOf[Type]
  }
}