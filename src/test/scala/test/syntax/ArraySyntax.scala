package test.syntax

import scala.collection.immutable.ArraySeq

object ArraySyntax {

  implicit class ToArraySeq[T](val array: Array[T]) extends AnyVal {

    def toImmutableArraySeq: ArraySeq[T] = ArraySeq.unsafeWrapArray(Array.copyOf(array, array.length))
  }
}
