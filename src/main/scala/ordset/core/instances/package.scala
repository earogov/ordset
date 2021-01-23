package ordset.core

import ordset.{Hash, Order, Show}
import ordset.core.domain.{AscOrder, DescOrder}
import ordset.core.domain.DirectedOrder.Wrapper
import ordset.util.label.Label

import scala.collection.immutable.Queue

package object instances {

  import cats.kernel

  object OrderLabels {

    val booleanDefault: Label = Label("BooleanDefault")

    val intDefault: Label = Label("IntDefault")

    val longDefault: Label = Label("LongDefault")

    val stringDefault: Label = Label("StringDefault")
  }

  object Boolean {

    implicit lazy val booleanAscOrder: AscOrder[Boolean] =
      new Wrapper(OrderLabels.booleanDefault, booleanOrder, booleanHash)

    lazy val booleanDescOrder: DescOrder[Boolean] = booleanAscOrder.reverse

    def booleanOrder: Order[Boolean] = kernel.instances.boolean.catsKernelStdOrderForBoolean

    def booleanHash: Hash[Boolean] = kernel.instances.boolean.catsKernelStdOrderForBoolean

    def booleanShow: Show[Boolean] = cats.instances.boolean.catsStdShowForBoolean
  }

  object Int {

    implicit lazy val intAscOrder: AscOrder[Int] =
      new Wrapper(OrderLabels.intDefault, intOrder, intHash)

    lazy val intDescOrder: DescOrder[Int] = intAscOrder.reverse

    implicit def intOrder: Order[Int] = kernel.instances.int.catsKernelStdOrderForInt

    implicit def intHash: Hash[Int] = kernel.instances.int.catsKernelStdOrderForInt

    implicit def intShow: Show[Int] = cats.instances.int.catsStdShowForInt
  }

  object Long {

    implicit lazy val longAscOrder: AscOrder[Long] =
      new Wrapper(OrderLabels.longDefault, longOrder, longHash)

    lazy val longDescOrder: DescOrder[Long] = longAscOrder.reverse

    implicit def longOrder: Order[Long] = kernel.instances.long.catsKernelStdOrderForLong

    implicit def longHash: Hash[Long] = kernel.instances.long.catsKernelStdOrderForLong

    implicit def longShow: Show[Long] = cats.instances.long.catsStdShowForLong
  }

  object String {

    implicit lazy val stringAscOrder: AscOrder[String] =
      new Wrapper(OrderLabels.stringDefault, stringOrder, stringHash)

    lazy val stringDescOrder: Order[String] = stringAscOrder.reverse

    implicit def stringOrder: Order[String] = kernel.instances.string.catsKernelStdOrderForString

    implicit def stringHash: Hash[String] = kernel.instances.string.catsKernelStdOrderForString

    implicit def stringShow: Show[String] = cats.instances.string.catsStdShowForString
  }

  object List {

    implicit def listOrder[T](implicit ev: Order[T]): Order[List[T]] =
      kernel.instances.list.catsKernelStdOrderForList

    implicit def listHash[T](implicit ev: Hash[T]): Hash[List[T]] =
      kernel.instances.list.catsKernelStdHashForList

    implicit def listShow[T](implicit ev: Show[T]): Show[List[T]] =
      cats.instances.list.catsStdShowForList
  }

  object LazyList {

    implicit def lazyListOrder[T](implicit ev: Order[T]): Order[LazyList[T]] =
      kernel.instances.lazyList.catsKernelStdOrderForLazyList

    implicit def lazyListHash[T](implicit ev: Hash[T]): Hash[LazyList[T]] =
      kernel.instances.lazyList.catsKernelStdHashForLazyList

    implicit def lazyListShow[T](implicit ev: Show[T]): Show[LazyList[T]] =
      cats.instances.lazyList.catsStdShowForLazyList
  }

  object Queue {

    implicit def queueOrder[T](implicit ev: Order[T]): Order[Queue[T]] =
      kernel.instances.queue.catsKernelStdOrderForQueue

    implicit def queueHash[T](implicit ev: Hash[T]): Hash[Queue[T]] =
      kernel.instances.queue.catsKernelStdHashForQueue

    implicit def queueShow[T](implicit ev: Show[T]): Show[Queue[T]] =
      cats.instances.queue.catsStdShowForQueue
  }

  object Set {

    implicit def setHash[T](): Hash[Set[T]] =
      kernel.instances.set.catsKernelStdHashForSet

    implicit def setShow[T](implicit ev: Show[T]): Show[Set[T]] =
      cats.instances.set.catsStdShowForSet
  }
}
