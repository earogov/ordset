package ordset.test

import ordset.Show

object AssertionsUtil {
  
  def debugInfo[E](
    expected: => E, 
    actual: => E, 
    info: => String = ""
  )(
    implicit show: Show[E]
  ): LazyClue =
    () => {
      val sep = System.lineSeparator
      val msg = s"${sep}expected: ${show.show(expected)}${sep}actual: ${show.show(actual)}"
      if (info.nonEmpty) msg + sep + info
      else msg
    }
}
