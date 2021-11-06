package ordset.util

/**
 * Thread unsafe wrapper for single value.
 *
 * ==Use case==
 *
 * Local variables can't be initialized with `_` syntax, so code below doesn't compile:
 * {{{
 *   def foo[V](seq: IterableOnce[V]): Unit = {
 *      val iter = seq.iterator
 *      var value: V = _
 *      while (iter.hasNext) {
 *         value = iter.next
 *      }
 *      println(value)
 *   }
 * }}}
 *
 * Compiles:
 * {{{
 *   def foo[V](seq: IterableOnce[V]): Unit = {
 *      val iter = seq.iterator
 *      val value = new ValueHolder[V]()
 *      while (iter.hasNext) {
 *         value.set(iter.next)
 *      }
 *      println(value.get)
 *   }
 * }}}
 *
 * Note, example is simplified, so other implementations without `_` syntax are possible.
 * But sometimes `_` is very handy.
 *
 * @tparam V type of value.
 */
protected[ordset] class ValueHolder[V]{

  private var value: V = _

  def get: V = value

  def set(v: V): Unit = {
    value = v
  }
}