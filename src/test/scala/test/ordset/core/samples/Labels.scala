package test.ordset.core.samples

import ordset.util.label.Label

object Labels {

  val uniformSeq: Label = Label("uniform sequence")

  val singleBoundedSeq: Label = Label("single bounded sequence")

  val multiBoundedSeq: Label = Label("multi bounded sequence")

  val degenerateSeq: Label = Label("degenerate sequence")

  val emptySet: Label = Label("empty set")

  val universalSet: Label = Label("universal set")

  def sample(n: Int): Label = Label(s"sample $n")

  def sample(n: String): Label = Label(s"sample $n")

  def seed(s: Int): Label = Label(s"seed $s")

  def seed(s: String): Label = Label(s"seed $s")
}
