package ordset.util.tag

trait TaggedBase[R, S <: R] {
  type Raw = R
  type Type = S with Tag[this.type]

  @inline final protected def fromRaw(value: Raw): Type = value.asInstanceOf[Type]
  @inline final protected def fromSubtype(value: S): Type = value.asInstanceOf[Type]
}