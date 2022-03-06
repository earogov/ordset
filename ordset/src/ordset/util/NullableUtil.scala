package ordset.util

protected[ordset] object NullableUtil {
  
  def nnOrElse[T](x: T | Null, default: T): T = if (x == null) default else x
}
