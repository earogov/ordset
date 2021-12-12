import cats.kernel
import scala.collection.immutable.Queue
import ordset.OrderComponents.*

package object ordset {

  type Direction = Direction.Type

  type Asc = Direction.Asc.Type

  type Desc = Direction.Desc.Type

  type Eq[A] = cats.kernel.Eq[A]

  type Hash[A] = cats.kernel.Hash[A]

  type Order[A] = cats.kernel.Order[A]

  type Show[A] = cats.Show[A]

  val Show: cats.Show.type = cats.Show

  val Hash: cats.Hash.type = cats.Hash

  val Order: cats.Order.type = cats.Order

  object instances {

    object unit {

      implicit val unitOrderWithHash: UnitDiscreteFiniteOrder = new UnitDiscreteFiniteOrder

      implicit def unitShow: Show[Unit] = cats.instances.unit.catsStdShowForUnit
    }

    object boolean {

      implicit val booleanOrderWithHash: BooleanDiscreteFiniteOrder = new BooleanDiscreteFiniteOrder

      implicit def booleanShow: Show[Boolean] = cats.instances.boolean.catsStdShowForBoolean
    }

    object byte {

      implicit val byteOrderWithHash: ByteDiscreteFiniteOrder = new ByteDiscreteFiniteOrder

      implicit def byteShow: Show[Byte] = cats.instances.byte.catsStdShowForByte
    }

    object short {

      implicit val shortOrderWithHash: ShortDiscreteFiniteOrder = new ShortDiscreteFiniteOrder

      implicit def shortShow: Show[Short] = cats.instances.short.catsStdShowForShort
    }

    object int {

      implicit val intOrderWithHash: IntDiscreteFiniteOrder = new IntDiscreteFiniteOrder

      implicit def intShow: Show[Int] = cats.instances.int.catsStdShowForInt
    }

    object long {

      implicit val longOrderWithHash: LongDiscreteFiniteOrder = new LongDiscreteFiniteOrder

      implicit def longShow: Show[Long] = cats.instances.long.catsStdShowForLong
    }

    object float {

      implicit val floatOrderWithHash: FloatBoundedOrder = new FloatBoundedOrder

      implicit def stringShow: Show[Float] = cats.instances.float.catsStdShowForFloat
    }

    object double {

      implicit val doubleOrderWithHash: DoubleBoundedOrder = new DoubleBoundedOrder

      implicit def stringShow: Show[Double] = cats.instances.double.catsStdShowForDouble
    }

    object string {

      implicit val stringOrderWithHash: StringBoundedBelowOrder = new StringBoundedBelowOrder

      implicit def stringShow: Show[String] = cats.instances.string.catsStdShowForString
    }

    object list {

      implicit def listOrderWithHash[T](implicit ord: Order[T], hash: Hash[T]): ListBoundedBelowOrder[T] =
        new ListBoundedBelowOrder

      implicit def listShow[T](implicit ev: Show[T]): Show[List[T]] = cats.instances.list.catsStdShowForList
    }

    object lazyList {

      implicit def lazyListOrderWithHash[T](implicit ord: Order[T], hash: Hash[T]): LazyListBoundedBelowOrder[T] =
        new LazyListBoundedBelowOrder

      implicit def lazyListShow[T](implicit ev: Show[T]): Show[LazyList[T]] =
        cats.instances.lazyList.catsStdShowForLazyList
    }

    object queue {

      implicit def queueOrderWithHash[T](implicit ord: Order[T], hash: Hash[T]): QueueBoundedBelowOrder[T] =
        new QueueBoundedBelowOrder

      implicit def queueShow[T](implicit ev: Show[T]): Show[Queue[T]] =
        cats.instances.queue.catsStdShowForQueue
    }

    object set {

      implicit def setHash[T](): Hash[Set[T]] =
        kernel.instances.set.catsKernelStdHashForSet

      implicit def setShow[T](implicit ev: Show[T]): Show[Set[T]] =
        cats.instances.set.catsStdShowForSet
    }

    object tuple2 {

      implicit def tuple2Hash[T1, T2](implicit ev1: Hash[T1], ev2: Hash[T2]): Hash[(T1, T2)] =
        kernel.instances.tuple.catsKernelStdHashForTuple2

      implicit def tuple2Show[T1, T2](implicit ev1: Show[T1], ev2: Show[T2]): Show[(T1, T2)] =
        cats.instances.tuple.catsStdShowForTuple2
    }

    object either {

      implicit def eitherHash[T1, T2](implicit ev1: Hash[T1], ev2: Hash[T2]): Hash[Either[T1, T2]] =
        kernel.instances.either.catsStdHashForEither

      implicit def eitherShow[T1, T2](implicit ev1: Show[T1], ev2: Show[T2]): Show[Either[T1, T2]] =
        cats.instances.either.catsStdShowForEither
    }
  }
}
