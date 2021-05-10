package test.ordset.core

import ordset.Show
import ordset.core.map.UniformOrderedMap
import ordset.core.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.util.label.Label

object Labels {

  val uniformSeq: Label = Label("uniform sequence")

  val singleBoundedSeq: Label = Label("single bounded sequence")

  val multiBoundedSeq: Label = Label("multi bounded sequence")

  val degenerateSeq: Label = Label("degenerate sequence")

  val emptySet: Label = Label("empty set")

  val universalSet: Label = Label("universal set")

  val uniformOrderedSet: Label = Label(classOf[UniformOrderedSet[_, _]].getSimpleName)

  val uniformOrderedMap: Label = Label(classOf[UniformOrderedMap[_, _, _]].getClass.getSimpleName)

  val arrayOrderedSet: Label = Label(classOf[ArrayOrderedSet[_, _]].getSimpleName)

  val treapOrderedSet: Label = Label(classOf[TreapOrderedSet[_, _]].getSimpleName)

  val zippedOrderedSet: Label = Label(classOf[ZippedOrderedSet[_, _, _, _]].getSimpleName)

  def sample(n: Int): Label = Label(s"sample $n")

  def sample(n: String): Label = Label(s"sample $n")

  def seed(s: Int): Label = Label(s"seed $s")

  def seed(s: Long): Label = Label(s"seed $s")

  def seed(s: String): Label = Label(s"seed $s")

  def caseLabel(c: Int): Label = Label(s"case $c")

  def caseLabel(c: String): Label = Label(s"case $c")

  val caseShow: Show[Set[Label]] =
    Label.customSetShow("case(", ", ", ")")(Label.defaultShow)
}
