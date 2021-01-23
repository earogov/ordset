package test.ordset.core.samples.treapOrderedSet

import ordset.core.TreapOrderedSet
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.syntax.BoundSyntax._
import ordset.util.RandomUtil
import ordset.util.label.Label
import test.ordset.core.behaviors._
import test.ordset.core.samples.Labels

import scala.collection.immutable.ArraySeq
import scala.language.postfixOps

class DegenerateSetSample1[D <: Domain[Int]](
  seed: Int
)(
  implicit override val domainOps: DomainOps[Int, D]
) extends TreapSegmentSeqSample[Int, D, Boolean](seed)
  with degenerateSet.Sample1[D] {

  override def labels: Set[Label] = super.labels + Labels.degenerateSeq

  override def sequence: GenSegmentSeq =
    TreapOrderedSet.fromIterable[Int, D](
      ArraySeq(0 `)[`, 0 `](`, 10 `)[`, 20 `)[`, 20 `](`, 30 `)[`, 30 `](`),
      RandomUtil.intLazyList(seed),
      complementary = false,
      domainOps.boundOrd.validateStrictly
    )
}
