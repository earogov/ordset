// package ordset.core.validation

// import scala.collection.{AbstractIterator, Iterator}
// import ordset.core.SegmentSeqException

// object ValidatedIterable {

//   opaque type Type[+E] = IterableOnce[E]

//   trait ValidatedIterator[+E] extends Iterator[E] {

//     @throws[NoSuchElementException, SegmentSeqException]
//     override def next(): E

//     def hasValidNext: Boolean
//   }

//   abstract class AbstractValidatedIterator[+E](
//     protected val iterator: Iterator[E]
//   ) extends AbstractIterator[E] with ValidatedIterator[E] {


//   }

//   class UnaryIterator[+E] extends Iterator[E] {

//   }

//   class BinaryIterator[+E] extends Iterator[E] {


//   }
// }