import cats.kernel
import scala.collection.immutable.Queue

package object ordset {

  type Eq[A] = cats.kernel.Eq[A]

  type Hash[A] = cats.kernel.Hash[A]

  type Order[A] = cats.kernel.Order[A]

  type Show[A] = cats.Show[A]

  val Show: cats.Show.type = cats.Show

  val Hash: cats.Hash.type = cats.Hash

  val Order: cats.Order.type = cats.Order

  object instances {

    object boolean {

      implicit def booleanOrder: Order[Boolean] = kernel.instances.boolean.catsKernelStdOrderForBoolean

      implicit def booleanHash: Hash[Boolean] = kernel.instances.boolean.catsKernelStdOrderForBoolean

      implicit def booleanShow: Show[Boolean] = cats.instances.boolean.catsStdShowForBoolean
    }

    object int {

      implicit def intOrder: Order[Int] = kernel.instances.int.catsKernelStdOrderForInt

      implicit def intHash: Hash[Int] = kernel.instances.int.catsKernelStdOrderForInt

      implicit def intShow: Show[Int] = cats.instances.int.catsStdShowForInt
    }

    object long {

      implicit def longOrder: Order[Long] = kernel.instances.long.catsKernelStdOrderForLong

      implicit def longHash: Hash[Long] = kernel.instances.long.catsKernelStdOrderForLong

      implicit def longShow: Show[Long] = cats.instances.long.catsStdShowForLong
    }

    object string {

      implicit def stringOrder: Order[String] = kernel.instances.string.catsKernelStdOrderForString

      implicit def stringHash: Hash[String] = kernel.instances.string.catsKernelStdOrderForString

      implicit def stringShow: Show[String] = cats.instances.string.catsStdShowForString
    }

    object list {

      implicit def listOrder[T](implicit ev: Order[T]): Order[List[T]] =
        kernel.instances.list.catsKernelStdOrderForList

      implicit def listHash[T](implicit ev: Hash[T]): Hash[List[T]] =
        kernel.instances.list.catsKernelStdHashForList

      implicit def listShow[T](implicit ev: Show[T]): Show[List[T]] =
        cats.instances.list.catsStdShowForList
    }

    object lazyList {

      implicit def lazyListOrder[T](implicit ev: Order[T]): Order[LazyList[T]] =
        kernel.instances.lazyList.catsKernelStdOrderForLazyList

      implicit def lazyListHash[T](implicit ev: Hash[T]): Hash[LazyList[T]] =
        kernel.instances.lazyList.catsKernelStdHashForLazyList

      implicit def lazyListShow[T](implicit ev: Show[T]): Show[LazyList[T]] =
        cats.instances.lazyList.catsStdShowForLazyList
    }

    object queue {

      implicit def queueOrder[T](implicit ev: Order[T]): Order[Queue[T]] =
        kernel.instances.queue.catsKernelStdOrderForQueue

      implicit def queueHash[T](implicit ev: Hash[T]): Hash[Queue[T]] =
        kernel.instances.queue.catsKernelStdHashForQueue

      implicit def queueShow[T](implicit ev: Show[T]): Show[Queue[T]] =
        cats.instances.queue.catsStdShowForQueue
    }

    object set {

      implicit def setHash[T](): Hash[Set[T]] =
        kernel.instances.set.catsKernelStdHashForSet

      implicit def setShow[T](implicit ev: Show[T]): Show[Set[T]] =
        cats.instances.set.catsStdShowForSet
    }
  }
}
