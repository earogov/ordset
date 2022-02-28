package ordset.test.core

import ordset.Show
import ordset.core.segmentSeq.map.UniformOrderedMap
import ordset.core.segmentSeq.set.{NonuniformArrayOrderedSet, NonuniformTreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
import ordset.test.Label
import ordset.test.Label.*
import ordset.test.core.implementations.segmentSeq.lazyTreap.LazyTreapSegmentSeq

object SegmentSeqLabels {

  val uniformSeq: Label = label("uniform sequence")

  val singleBoundedSeq: Label = label("single bounded sequence")

  val multiBoundedSeq: Label = label("multi bounded sequence")

  val degenerateSeq: Label = label("degenerate sequence")

  val emptySet: Label = label("empty set")

  val universalSet: Label = label("universal set")

  val uniformOrderedSet: Label = label("uniform set")

  val uniformOrderedMap: Label = label("uniform map")

  val arrayOrderedSet: Label = label("array set")

  val treapOrderedSet: Label = label("treap set")

  val lazyTreapOrderedSet: Label = label("lazy treap set")

  val zippedOrderedSet: Label = label("zipped set")
}
