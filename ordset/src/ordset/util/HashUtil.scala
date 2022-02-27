package ordset.util

import ordset.Hash
import scala.util.hashing.MurmurHash3._

protected[ordset] object HashUtil {

  val product1Hash: Int => Int = cats.kernel.instances.StaticMethods.product1Hash

  val product2Hash: (Int, Int) => Int = cats.kernel.instances.StaticMethods.product2Hash

  val product3Hash: (Int, Int, Int) => Int = 
    (h1, h2, h3) => {
      var h = productSeed
      h = mix(h, h1)
      h = mix(h, h2)
      h = mix(h, h3)
      finalizeHash(h, 3)
    }

  val product4Hash: (Int, Int, Int, Int) => Int = 
    (h1, h2, h3, h4) => {
      var h = productSeed
      h = mix(h, h1)
      h = mix(h, h2)
      h = mix(h, h3)
      h = mix(h, h4)
      finalizeHash(h, 4)
    }

  val product5Hash: (Int, Int, Int, Int, Int) => Int = 
    (h1, h2, h3, h4, h5) => {
      var h = productSeed
      h = mix(h, h1)
      h = mix(h, h2)
      h = mix(h, h3)
      h = mix(h, h4)
      h = mix(h, h5)
      finalizeHash(h, 5)
    }

  val product6Hash: (Int, Int, Int, Int, Int, Int) => Int = 
    (h1, h2, h3, h4, h5, h6) => {
      var h = productSeed
      h = mix(h, h1)
      h = mix(h, h2)
      h = mix(h, h3)
      h = mix(h, h4)
      h = mix(h, h5)
      h = mix(h, h6)
      finalizeHash(h, 6)
    }

  val product7Hash: (Int, Int, Int, Int, Int, Int, Int) => Int = 
    (h1, h2, h3, h4, h5, h6, h7) => {
      var h = productSeed
      h = mix(h, h1)
      h = mix(h, h2)
      h = mix(h, h3)
      h = mix(h, h4)
      h = mix(h, h5)
      h = mix(h, h6)
      h = mix(h, h7)
      finalizeHash(h, 7)
    }

  val product8Hash: (Int, Int, Int, Int, Int, Int, Int, Int) => Int = 
    (h1, h2, h3, h4, h5, h6, h7, h8) => {
      var h = productSeed
      h = mix(h, h1)
      h = mix(h, h2)
      h = mix(h, h3)
      h = mix(h, h4)
      h = mix(h, h5)
      h = mix(h, h6)
      h = mix(h, h7)
      h = mix(h, h8)
      finalizeHash(h, 8)
    }

  val product9Hash: (Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = 
    (h1, h2, h3, h4, h5, h6, h7, h8, h9) => {
      var h = productSeed
      h = mix(h, h1)
      h = mix(h, h2)
      h = mix(h, h3)
      h = mix(h, h4)
      h = mix(h, h5)
      h = mix(h, h6)
      h = mix(h, h7)
      h = mix(h, h8)
      h = mix(h, h9)
      finalizeHash(h, 9)
    }

  val product10Hash: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int = 
    (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10) => {
      var h = productSeed
      h = mix(h, h1)
      h = mix(h, h2)
      h = mix(h, h3)
      h = mix(h, h4)
      h = mix(h, h5)
      h = mix(h, h6)
      h = mix(h, h7)
      h = mix(h, h8)
      h = mix(h, h9)
      h = mix(h, h10)
      finalizeHash(h, 10)
    }

  /**
   * Returns [[Hash]] instance such that: 
   * <div>- two objects `x` and `y` are equal iff they are equal according to `hash1` and `hash2`;</div>
   * <div>- hash of object `x` is calculated as a mix of hashes returned by `hash1` and `hash2`.</div>
   */
  def composeHash[E1, E2, E](hash1: Hash[E1], hash2: Hash[E2], f1: E => E1, f2: E => E2): Hash[E] = 
    new ComposedHash(hash1, hash2, f1, f2)

  /**
   * Returns [[Hash]] instance such that: 
   * <div>- two objects `x` and `y` are equal iff they have the same class name;</div>
   * <div>- hash of object `x` is calculated by its class name.</div>
   */
  def classBasedHash[E]: Hash[E] = ClassBasedHash.asInstanceOf[Hash[E]]

  // Private section ---------------------------------------------------------- //
  private final case class ComposedHash[E1, E2, E](
    hash1: Hash[E1],
    hash2: Hash[E2],
    f1: E => E1,
    f2: E => E2
  ) extends Hash[E] {

    override def eqv(x: E, y: E): Boolean = hash1.eqv(f1(x), f1(y)) && hash2.eqv(f2(x), f2(y))

    override def hash(x: E): Int = product2Hash(hash1.hash(f1(x)), hash2.hash(f2(x)))
  }

  private object ClassBasedHash extends Hash[Any] {

    override def eqv(x: Any, y: Any): Boolean = x.getClass().getName() == y.getClass().getName()

    override def hash(x: Any): Int = product1Hash(x.getClass().getName().hashCode)
  }
}
