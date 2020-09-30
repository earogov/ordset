package ordset

import ordset.domain.{AscOrder, DescOrder}
import ordset.domain.DirectedOrder.Wrapper
import ordset.util.label.Label

package object instances {

  import cats.kernel

  object OrderLabels {

    val booleanDefault: Label = Label("BooleanDefault")

    val intDefault: Label = Label("IntDefault")

    val stringDefault: Label = Label("StringDefault")
  }

  object Boolean {

    implicit lazy val booleanAscOrder: AscOrder[Boolean] =
      new Wrapper(OrderLabels.booleanDefault, booleanOrder, booleanHash)

    lazy val booleanDescOrder: DescOrder[Boolean] = booleanAscOrder.reverse

    def booleanOrder: Order[Boolean] = kernel.instances.boolean.catsKernelStdOrderForBoolean

    def booleanHash: Hash[Boolean] = kernel.instances.boolean.catsKernelStdOrderForBoolean
  }

  object Int {

    implicit lazy val intAscOrder: AscOrder[Int] =
      new Wrapper(OrderLabels.intDefault, intOrder, intHash)

    lazy val intDescOrder: DescOrder[Int] = intAscOrder.reverse

    def intOrder: Order[Int] = kernel.instances.int.catsKernelStdOrderForInt

    def intHash: Hash[Int] = kernel.instances.int.catsKernelStdOrderForInt
  }

  object String {

    implicit lazy val stringAscOrder: AscOrder[String] =
      new Wrapper(OrderLabels.stringDefault, stringOrder, stringHash)

    lazy val stringDescOrder: Order[String] = stringAscOrder.reverse

    def stringOrder: Order[String] = kernel.instances.string.catsKernelStdOrderForString

    def stringHash: Hash[String] = kernel.instances.string.catsKernelStdOrderForString
  }

  object LazyList {

    def lazyListOrder[T](implicit ev: Order[T]): Order[LazyList[T]] =
      kernel.instances.lazyList.catsKernelStdOrderForLazyList

    def lazyListHash[T](implicit ev: Hash[T]): Hash[LazyList[T]] =
      kernel.instances.lazyList.catsKernelStdHashForLazyList
  }
}
