package ordset

object HashComponents {

  trait HashProxy[E] extends Hash[E] {

    protected val original: Hash[E]

    override def hash(x: E): Int = original.hash(x)
  }
}