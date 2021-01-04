package ordset.tree.core

object Validation {

  trait KeyOrderFunc[-K] extends ((K, K) => Boolean) {
    override def apply(prev: K, next: K ): Boolean
  }
}
