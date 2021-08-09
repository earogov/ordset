//package ordset.core.map
//
//import ordset.core.AbstractLazyTreapSegmentSeq.{BaseSegmentSeq, EagerValue, LazySegmentBase, ZSegmentSeq}
//import ordset.core.{AbstractLazyTreapSegmentSeq, Bound, LazySegmentSeq, SegmentSeq}
//import ordset.core.domain.{Domain, DomainOps}
//import ordset.core.value.ValueOps
//import ordset.random.RngManager
//import ordset.tree.treap.immutable.ImmutableTreap
//
//class LazyTreapOrderedMap[E, D <: Domain[E], V] protected (
//  baseMap: OrderedMap[E, D, V],
//  lazyMap: OrderedMap[E, D, Option[() => OrderedMap[E, D, V]]]
//)(
//  implicit
//  final override val domainOps: DomainOps[E, D],
//  final override val valueOps: ValueOps[V],
//  final override val rngManager: RngManager
//) extends AbstractLazyTreapSegmentSeq[E, D, V]
//  with OrderedMapCommons[E, D, V, LazySegmentBase[E, D, V]] {
//
//  // Initialization ----------------------------------------------------------- //
//  //zippedSeq = initializeZippedSeq(baseMap, lazyMap)
//
//  // Protected section -------------------------------------------------------- //
//  @inline
//  protected final override def consUniform(value: V): UniformOrderedMap[E, D, V] = UniformOrderedMap.default(value)
//
//  protected final override def consLazy(zippedSeq: ZSegmentSeq[E, D, V]): LazySegmentSeq[E, D, V] = ???
//
////  protected final def initializeZippedSeq(
////    baseMap: OrderedMap[E, D, V],
////    lazyMap: OrderedMap[E, D, Option[() => OrderedMap[E, D, V]]]
////  ): ZSegmentSeq[E, D, V] = {
////    val initZippedSeq = makeZippedSeq(
////      TreapOrderedMap.getFactory.convertMap(baseMap),
////      makeUniformControlSeq(EagerValue(true))
////    )
////    lazyMap.firstSegment.forwardIterator.foldLeft(initZippedSeq) { (zippedSeq, lazySegment) =>
////        lazySegment.value
////          .map { lazyValue =>
//////            val flippedLazyLower = lazySegment.lowerExtended.flipLimited
//////            val lowerZsegment = zippedSeq.getSegmentForExtended(flippedLazyLower)
//////            val tmpZippedSeq = appendLazyAboveExtended(flippedLazyLower, lowerZsegment, lazyValue)
//////            val flippedLazyUpper = lazySegment.upperExtended.flipLimited
//////            val upperZsegment = tmpZippedSeq.getSegmentForExtended(flippedLazyUpper)
//////            prependBelowExtended(flippedLazyUpper, upperZsegment, lazyValue)
////          }
////          .getOrElse(zippedSeq)
////    }
////    zippedSeq
////  }
//}
