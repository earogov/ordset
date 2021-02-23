package ordset.core

import ordset.Order
import ordset.core.domain.DirectedOrder.Wrapper
import ordset.core.domain.{AscOrder, DescOrder}
import ordset.util.label.Label

package object instances {

  object OrderLabels {

    val booleanDefault: Label = Label("BooleanDefault")

    val intDefault: Label = Label("IntDefault")

    val longDefault: Label = Label("LongDefault")

    val stringDefault: Label = Label("StringDefault")
  }

  object boolean {

    import ordset.instances.boolean._

    implicit lazy val booleanAscOrder: AscOrder[Boolean] =
      new Wrapper(OrderLabels.booleanDefault, booleanOrder, booleanHash)

    lazy val booleanDescOrder: DescOrder[Boolean] = booleanAscOrder.reverse
  }

  object int {

    import ordset.instances.int._

    implicit lazy val intAscOrder: AscOrder[Int] =
      new Wrapper(OrderLabels.intDefault, intOrder, intHash)

    lazy val intDescOrder: DescOrder[Int] = intAscOrder.reverse
  }

  object long {

    import ordset.instances.long._

    implicit lazy val longAscOrder: AscOrder[Long] =
      new Wrapper(OrderLabels.longDefault, longOrder, longHash)

    lazy val longDescOrder: DescOrder[Long] = longAscOrder.reverse
  }

  object string {

    import ordset.instances.string._

    implicit lazy val stringAscOrder: AscOrder[String] =
      new Wrapper(OrderLabels.stringDefault, stringOrder, stringHash)

    lazy val stringDescOrder: Order[String] = stringAscOrder.reverse
  }
}
