//package test.ordset.core.behaviors.lazyTreapSeq.singleBoundedSet
//
//import ordset.core.AbstractLazyTreapSegmentSeq.EagerValue
//import ordset.core.ExtendedBound
//import ordset.core.domain.{Domain, DomainOps}
//import ordset.core.set.{ArrayOrderedSet, TreapOrderedSet, UniformOrderedSet, ZippedOrderedSet}
//import ordset.core.syntax.BoundSyntax._
//import ordset.core.syntax.SetBuilderNotation._
//import ordset.random.RngManager
//import ordset.util.label.Label
//import test.ordset.core.Labels
//import test.ordset.core.behaviors.lazyTreapSeq.LazyTreapSeqCacheTest
//import test.ordset.core.behaviors.zippedSeq.OriginalSeqPatchTest
//import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample
//
//import scala.collection.immutable.ArraySeq
//import scala.language.postfixOps
//
//trait SampleLT1[D <: Domain[Int]]
//  extends LazyTreapSeqCacheTest[Int, D, Boolean] {
//  self: LazyTreapSeqSample[Int, D, Boolean] =>
//
//  override val sample: String = "LT1"
//
//  // Protected section -------------------------------------------------------- //
//
//  // X----------------------------f--------------------------------X  seq1
//  //
//  // X---------------f------------)[---------------t---------------X  seq2
//  //                               0
//  //
//  // X----------------------------t--------------------------------X  seq3
//  //
//  //        seq1                 seq2                  seq3
//  // X-----------------)[--------------------](--------------------X
//  //                  -10                    10
//  override protected def initializeSequence: LazyTreapSeqSample.LazyTreapSegmentSeq[Int, D, Boolean] = {
//    val seq1 = TreapOrderedSet.getFactory[Int, D].unsafeBuildAsc(ArraySeq.empty, complementary = false, domainOps)()
//    val seq2 = TreapOrderedSet.getFactory[Int, D].unsafeBuildAsc(ArraySeq(0 `)[`), complementary = false, domainOps)()
//    val seq3 = TreapOrderedSet.getFactory[Int, D].unsafeBuildAsc(ArraySeq.empty, complementary = true, domainOps)()
//
//    new LazyTreapSeqSample.LazyTreapSegmentSeq(
//      List(
//        (-10 `)`, () => seq1),
//        ( 10 `]`, () => seq2),
//        (ExtendedBound.AboveAll, () => seq3)
//      )
//    )
//  }
//
//  override def lazyCacheCases: Iterable[LazyTreapSeqCacheTest.TestPackage[Int, D, Boolean]] = List(
//    LazyTreapSeqCacheTest.TestPackage(
//      Set(Label("A")),
//      List(
//        LazyTreapSeqCacheTest.TestCase(
//          -5 `)`,
//          false forAll x,
//          List(
//            someLazyZvalue forAll (x < -10)
//            (false, EagerValue.stable[Int, D, Boolean]) forAll x
//          )
//        ),
//        LazyTreapSeqCacheTest.TestCase(
//          15`]`,
//          false forAll x,
//          List(
//            (false, EagerValue.stable[Int, D, Boolean]) forAll x
//          )
//        )
//      )
//    )
//  )
//}
