package ordset.random.commons.internal

import org.apache.commons.rng.core.util.NumberFactory

private[commons] object NumbersUtil {

  def makeIntArray(long1: Long, long2: Long): Array[Int] = {
    val array = new Array[Int](4)
    array(0) = NumberFactory.extractHi(long1)
    array(1) = NumberFactory.extractLo(long1)
    array(2) = NumberFactory.extractHi(long2)
    array(3) = NumberFactory.extractLo(long2)
    array
  }
}
