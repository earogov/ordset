package test.ordset.core.samples.treapOrderedSet

import ordset.core.TreapOrderedSet
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.RandomUtil
import ordset.util.label.Label
import test.ordset.core.behaviors._
import test.ordset.core.samples.Labels

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class UniversalSetSample1[D <: Domain[Int]](
  seed: Int
)(
  implicit override val domainOps: DomainOps[Int, D]
) extends TreapSegmentSeqSample[Int, D, Boolean](seed)
  with universalSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.universalSet

  override def sequence: GenSegmentSeq =
    TreapOrderedSet.fromIterable[Int, D](
      ArraySeq.empty,
      RandomUtil.intLazyList(seed),
      complementary = true,
      domainOps.boundOrd.validateStrictly
    )
}
