package ordset.util

import ordset.Show

protected[ordset] object ShowUtil {

  val singleIndent: String = "  "

  def standardShow[T](name: String)(fieldsFunc: T => List[(String, String)]): Show[T] = 
    new StandardShow(name, fieldsFunc)

  def fieldShow[T](show: Show[T]): Show[T] = 
    new FieldShow(show)

  // Private section ---------------------------------------------------------- //
  private val fieldsBegin: String = "("
  private val fieldsEnd: String = ")"
  private val fieldNameSep: String = ": "
  private val fieldIndent: String = getIndent(2)

  private def getIndent(level: Int): String = singleIndent * level

  private final case class StandardShow[T] (
    val name: String,
    val fieldsFunc: T => List[(String, String)],
    val indent: String = ""
  ) extends Show[T] {

    override def show(t: T): String= {
      val b = new StringBuilder()
      b.append(System.lineSeparator)
      b.append(indent)
      b.append(name)
      b.append(fieldsBegin)
      fieldsFunc(t).foreach { f =>
        b.append(System.lineSeparator)
        b.append(indent)
        b.append(singleIndent)
        b.append(f._1)
        b.append(fieldNameSep)
        b.append(f._2)
      }
      b.append(System.lineSeparator)
      b.append(indent)
      b.append(fieldsEnd)
      b.result()
    }

    def withIndent(indent: String): StandardShow[T] = new StandardShow(name, fieldsFunc, indent)
  }

  private final case class FieldShow[T] (
    val show: Show[T]
  ) extends Show[T] {

    override def show(t: T): String = show match {
      case show: StandardShow[T] => 
        show.withIndent(fieldIndent).show(t)
      case _ =>
        import scala.language.unsafeNulls
        val lines: Array[String] = show.show(t).split(System.lineSeparator)
        val indentedLines = lines.zipWithIndex.map { (line, index) => 
          if (index == 0) line else fieldIndent + line
        }
        indentedLines.mkString(System.lineSeparator)
    }
  }
}