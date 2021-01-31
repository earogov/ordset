package test.ordset.core

import cats.Show
import ordset.core.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedMap, UniformOrderedSet, ZippedOrderedSet}
import ordset.util.label.Label

object Labels {

  val uniformSeq: Label = Label("uniform sequence")

  val singleBoundedSeq: Label = Label("single bounded sequence")

  val multiBoundedSeq: Label = Label("multi bounded sequence")

  val degenerateSeq: Label = Label("degenerate sequence")

  val emptySet: Label = Label("empty set")

  val universalSet: Label = Label("universal set")

  val uniformOrderedSet: Label = Label(UniformOrderedSet.getClass.getSimpleName)

  val uniformOrderedMap: Label = Label(UniformOrderedMap.getClass.getSimpleName)

  val arrayOrderedSet: Label = Label(ArrayOrderedSet.getClass.getSimpleName)

  val treapOrderedSet: Label = Label(TreapOrderedSet.getClass.getSimpleName)

  val zippedOrderedSet: Label = Label(ZippedOrderedSet.getClass.getSimpleName)

  def sample(n: Int): Label = Label(s"sample $n")

  def sample(n: String): Label = Label(s"sample $n")

  def seed(s: Int): Label = Label(s"seed $s")

  def seed(s: String): Label = Label(s"seed $s")

  def caseLabel(c: Int): Label = Label(s"case $c")

  def caseLabel(c: String): Label = Label(s"case $c")

  val caseShow: Show[Set[Label]] =
    Label.customSetShow("case(", ", ", ")")(Label.defaultShow)
}
