package ordset.util

import ordset.Show

protected[ordset] object ShowUtil {

  def standardShow[T](name: String)(fieldsFunc: T => List[(String, String)]): Show[T] = 
    new StandardShow(name, fieldsFunc)

  // Private section ---------------------------------------------------------- //
  private final case class StandardShow[T] (
    val name: String,
    val fieldsFunc: T => List[(String, String)]
  ) extends Show[T] {

    override def show(t: T): String= {
      val stringBuilder = new StringBuilder()
      stringBuilder.append(s"\n$name(")
      fieldsFunc(t).foreach(f => stringBuilder.append(s"\n  ${f._1}: ${f._2}"))
      stringBuilder.append("\n)")
      stringBuilder.result()
    }
  }
}