package ordset.util

protected[ordset] object OptionUtil {

  /**
   * Returns same result as `Option(x)`. 
   *
   * `Option(x)` doesn't compile with `-Yexplicit-nulls` flag.
   */
  def optionOfNullable[T](x: T | Null): Option[T] = 
    if x != null then Some(x)
    else None
}