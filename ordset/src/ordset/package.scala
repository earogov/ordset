import cats.kernel
import scala.collection.immutable.Queue

package object ordset {

  type Eq[A] = cats.kernel.Eq[A]

  type Hash[A] = cats.kernel.Hash[A]

  type Order[A] = cats.kernel.Order[A]

  type Show[A] = cats.Show[A]

  type ContravariantShow[A] = cats.Show.ContravariantShow[A]

  val Show: cats.Show.type = cats.Show

  val Hash: cats.Hash.type = cats.Hash

  val Order: cats.Order.type = cats.Order

  object givens {

    object unit {

      import implementations.unit._

      implicit val unitNaturalOrder: NaturalOrder = new NaturalOrder

      implicit def unitShow: Show[Unit] = cats.instances.unit.catsStdShowForUnit
    }

    object boolean {

      import implementations.boolean._

      implicit val booleanNaturalOrder: NaturalOrder = new NaturalOrder

      implicit def booleanShow: Show[Boolean] = cats.instances.boolean.catsStdShowForBoolean
    }

    object byte {

      import implementations.byte._

      implicit val byteNaturalOrder: NaturalOrder = new NaturalOrder

      implicit def byteShow: Show[Byte] = cats.instances.byte.catsStdShowForByte
    }

    object short {

      import implementations.short._

      implicit val shortNaturalOrder: NaturalOrder = new NaturalOrder

      implicit def shortShow: Show[Short] = cats.instances.short.catsStdShowForShort
    }

    object int {

      import implementations.int._

      implicit val intNaturalOrder: NaturalOrder = new NaturalOrder

      implicit def intShow: Show[Int] = cats.instances.int.catsStdShowForInt
    }

    object long {

      import implementations.long._

      implicit val longNaturalOrder: NaturalOrder = new NaturalOrder

      implicit def longShow: Show[Long] = cats.instances.long.catsStdShowForLong
    }

    object float {

      import implementations.float._

      implicit val floatNaturalOrder: NaturalOrder = new NaturalOrder

      implicit def stringShow: Show[Float] = cats.instances.float.catsStdShowForFloat
    }

    object double {

      import implementations.double._

      implicit val doubleNaturalOrder: NaturalOrder = new NaturalOrder

      implicit def stringShow: Show[Double] = cats.instances.double.catsStdShowForDouble
    }

    object string {

      import implementations.string._

      implicit val stringNaturalOrder: NaturalOrder = new NaturalOrder

      implicit def stringShow: Show[String] = cats.instances.string.catsStdShowForString
    }

    object list {

      import implementations.list._

      implicit def listNaturalOrder[T](implicit ord: Order[T], hash: Hash[T]): NaturalOrder[T] = new NaturalOrder

      implicit def listShow[T](implicit ev: Show[T]): Show[List[T]] = cats.instances.list.catsStdShowForList
    }

    object lazyList {

      import implementations.lazyList._

      implicit def lazyListNaturalOrder[T](implicit ord: Order[T], hash: Hash[T]): NaturalOrder[T] = new NaturalOrder

      implicit def lazyListShow[T](implicit ev: Show[T]): Show[LazyList[T]] =
        cats.instances.lazyList.catsStdShowForLazyList
    }

    object queue {

      import implementations.queue._

      implicit def queueNaturalOrder[T](implicit ord: Order[T], hash: Hash[T]): NaturalOrder[T] = new NaturalOrder

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
