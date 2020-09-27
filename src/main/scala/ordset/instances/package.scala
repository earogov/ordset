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
      new Wrapper(
        OrderLabels.booleanDefault,
        kernel.instances.boolean.catsKernelStdOrderForBoolean,
        kernel.instances.boolean.catsKernelStdOrderForBoolean
      )

    lazy val booleanDescOrder: DescOrder[Boolean] = booleanAscOrder.reverse

    lazy val booleanHash: Hash[Boolean] = kernel.instances.boolean.catsKernelStdOrderForBoolean
  }

  object Int {

    implicit lazy val intAscOrder: AscOrder[Int] =
      new Wrapper(
        OrderLabels.intDefault,
        kernel.instances.int.catsKernelStdOrderForInt,
        kernel.instances.int.catsKernelStdOrderForInt
      )

    lazy val intDescOrder: DescOrder[Int] = intAscOrder.reverse

    lazy val intHash: Hash[Int] = kernel.instances.int.catsKernelStdOrderForInt
  }

  object String {

    implicit lazy val stringAscOrder: AscOrder[String] = //kernel.instances.string.catsKernelStdOrderForString
      new Wrapper(
        OrderLabels.stringDefault,
        kernel.instances.string.catsKernelStdOrderForString,
        kernel.instances.string.catsKernelStdOrderForString
      )

    lazy val stringDescOrder: Order[String] = stringAscOrder.reverse

    lazy val stringHash: Hash[String] = kernel.instances.string.catsKernelStdOrderForString
  }
}
