package ordset.util

import scala.util.Random

object RandomUtil {

  def intLazyList(seed: Int): LazyList[Int] = {
    val rnd = new Random(seed)
    def loop(): LazyList[Int] = LazyList.cons(rnd.nextInt(), loop())
    loop()
  }

  def longLazyList(seed: Long): LazyList[Long] = {
    val rnd = new Random(seed)
    def loop(): LazyList[Long] = LazyList.cons(rnd.nextLong(), loop())
    loop()
  }
}
