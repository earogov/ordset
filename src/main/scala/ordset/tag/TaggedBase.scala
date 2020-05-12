package ordset.tag

trait TaggedBase[R] {
  type Raw = R
  type Type = Raw with Tag[this.type]

  @inline final protected def fromRaw(r: Raw): Type = r.asInstanceOf[Type]
}