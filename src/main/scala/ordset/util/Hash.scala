package ordset.util

private[ordset] object Hash {

  val product1Hash: Int => Int = cats.kernel.instances.StaticMethods.product1Hash
  val product2Hash: (Int, Int) => Int = cats.kernel.instances.StaticMethods.product2Hash
  val product3Hash: (Int, Int, Int) => Int = (x, y, z) => product2Hash(x, product2Hash(y, z))
}
