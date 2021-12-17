package ordset.test

object AssertionsUtil {
  
  def debugInfo(expected: Any, actual: Any, info: String = ""): String = {
    val sep = System.lineSeparator
    val msg = s"${sep}expected: $expected${sep}actual: $actual"
    if (info.nonEmpty) msg + sep + info
    else msg
  }
}
