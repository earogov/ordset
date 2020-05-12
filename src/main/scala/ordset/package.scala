package object ordset {

  type Order[A] = cats.kernel.Order[A]
  type Eq[A] = cats.kernel.Eq[A]
  type Hash[A] = cats.kernel.Hash[A]
  type Discrete[A] = cats.collections.Discrete[A]

  val IntAscOrder: Order[Int] = cats.kernel.instances.int.catsKernelStdOrderForInt
}
