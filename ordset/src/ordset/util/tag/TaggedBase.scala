package ordset.util.tag

trait TaggedBase[R, L <: R] {
  type Raw = R
  type Type = L with Tag[this.type]

  @inline final protected def fromRaw(value: Raw): Type = value.asInstanceOf[Type]
  @inline final protected def fromLub(value: L): Type = value.asInstanceOf[Type]
}