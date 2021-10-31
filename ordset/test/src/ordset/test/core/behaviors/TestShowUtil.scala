package ordset.test.core.behaviors

import ordset.core.ExtendedBound

object TestShowUtil {
  
  def caseWithBoundToString[E](bound: ExtendedBound[E]): String = s"case(bound: $bound)"
}
