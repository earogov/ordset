package ordset.util

object HashUtil {

  val product1Hash: Int => Int = cats.kernel.instances.StaticMethods.product1Hash

  val product2Hash: (Int, Int) => Int = cats.kernel.instances.StaticMethods.product2Hash

  val product3Hash: (Int, Int, Int) => Int = (h1, h2, h3) => {
    import scala.util.hashing.MurmurHash3._
    var h = productSeed
    h = mix(h, h1)
    h = mix(h, h2)
    h = mix(h, h3)
    finalizeHash(h, 3)
  }

  val product4Hash: (Int, Int, Int, Int) => Int = (h1, h2, h3, h4) => {
    import scala.util.hashing.MurmurHash3._
    var h = productSeed
    h = mix(h, h1)
    h = mix(h, h2)
    h = mix(h, h3)
    h = mix(h, h4)
    finalizeHash(h, 4)
  }
}
