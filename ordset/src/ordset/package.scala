package object ordset {

  type Eq[A] = cats.kernel.Eq[A]

  type Hash[A] = cats.kernel.Hash[A]

  type Order[A] = cats.kernel.Order[A]

  type Discrete[A] = cats.collections.Discrete[A]

  type Show[A] = cats.Show[A]

  val Show: cats.Show.type = cats.Show

  val Hash: cats.Hash.type = cats.Hash

  val Order: cats.Order.type = cats.Order
}
