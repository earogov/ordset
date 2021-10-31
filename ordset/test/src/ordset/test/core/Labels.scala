package ordset.test.core

import ordset.Show
import ordset.core.map.UniformOrderedMap
import ordset.core.set.{NonuniformArrayOrderedSet, NonuniformTreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.util.label.Label
import ordset.test.core.implementations.segmentSeq.lazyTreap.LazyTreapSegmentSeq

object Labels {

  val uniformSeq: Label = Label("uniform sequence")

  val singleBoundedSeq: Label = Label("single bounded sequence")

  val multiBoundedSeq: Label = Label("multi bounded sequence")

  val degenerateSeq: Label = Label("degenerate sequence")

  val emptySet: Label = Label("empty set")

  val universalSet: Label = Label("universal set")

  val uniformOrderedSet: Label = Label("uniform set")

  val uniformOrderedMap: Label = Label("uniform map")

  val arrayOrderedSet: Label = Label("array set")

  val treapOrderedSet: Label = Label("treap set")

  val lazyTreapOrderedSet: Label = Label("lazy treap set")

  val zippedOrderedSet: Label = Label("zipped set")

  def sample(n: Int): Label = Label(s"sample $n")

  def sample(n: String): Label = Label(s"sample $n")

  def seed(s: Int): Label = Label(s"seed $s")

  def seed(s: Long): Label = Label(s"seed $s")

  def seed(s: String): Label = Label(s"seed $s")

  def caseLabel(c: Int): Label = Label(s"case $c")

  def caseLabel(c: String): Label = Label(s"case $c")

  val caseShow: Show[Set[Label]] =
    Label.customSetShow("case(", ", ", ")")(Label.defaultShow)

  val packageShow: Show[Set[Label]] =
    Label.customSetShow("package(", ", ", ")")(Label.defaultShow)
}
