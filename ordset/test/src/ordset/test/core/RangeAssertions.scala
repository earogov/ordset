package ordset.test.core

import ordset.{Hash, Show}
import ordset.core.range.Range

object RangeAssertions {
    
  import ordset.test.AssertionsUtil.debugInfo
  import org.scalatest.Assertions._

  def assertSameRange[E, R <: Range[E]](
    expected: R,
    actual: R,
    info: => String = ""
  )(
    implicit 
    rangeHash: Hash[R],
    rangeShow: Show[R]
  ): Unit =
    assert(
      rangeHash.eqv(expected, actual),
      debugInfo(expected, actual, info)(rangeShow)
    )
}
