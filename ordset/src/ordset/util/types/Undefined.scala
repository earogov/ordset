package ordset.util.types

/**
 * Equivalent of `null`. May be used to specify uninitialized value:
 * {{{
 *   var x: String | Undefined.type = Undefined
 *   ...
 *   x = "someValue"
 * }}}
 * 
 * Using `Undefined` has the following advantage over `null`: 
 * - it's accessible only inside current project, so we can't receive `Undefined` from the client code. 
 * 
 * Consider simplified case:
 * {{{
 * 
 *    class Foo[E](f: () => E) {
 * 
 *      private var v: E | Null = null
 * 
 *      def getValue(): E =
 *        if (v != null) v
 *        else {
 *          val tmp = f.apply()
 *          v = tmp
 *          tmp
 *        }
 *    }
 * }}}
 * 
 * Given implementation can't guarantee, that function `f` is called only once. When `f` returns `null`, we can't
 * distinguish it from initial `null` and call `f` repeatedly to initialize `v`. On the other hand, `f` can't return
 * `Undefined`, so it will be a better choice:
 * {{{
 * 
 *   class Foo[E](f: () => E) {
 * 
 *     private var v: E | Undefined.type  = Undefined
 * 
 *     def getValue(): E =
 *       if (Undefined != v) Undefined.asDefined(v)
 *       else {
 *         val tmp = f.apply()
 *         v = tmp
 *         tmp
 *       }
 *   }
 * }}}
 */
protected[ordset] object Undefined {

  /**
   * Unconditionally cast away the `Undefined.type` part of union type.
   * 
   * Use only after check:
   * {{{
   *   
   *   if (Undefined != v) Undefined.asDefined(v)
   *   else ???
   * }}}
   */
  def asDefined[E](x: Undefined[E]): E = x.asInstanceOf[E]

  override def toString(): String = "Undefined"
}
