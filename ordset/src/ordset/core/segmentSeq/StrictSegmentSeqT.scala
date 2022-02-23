package ordset.core.segmentSeq

import ordset.core.domain.Domain

/**
 * According to evaluation strategy the following types of segment sequences are distinguished:
 * <tr>- strict</tr>
 * <tr>- lazy</tr>
 * 
 * Lazy sequences perform some additional computations to return segment or value. For instance, mapped sequence 
 * receives segment of original sequence and then applies mapping function to its value; zipped sequence gets
 * segments of both original sequences and return tuple of values. 
 * 
 * In contrast strict sequence stores segments in some data structure which provides efficient access, and when 
 * segment is requested only search operation is performed (see [[AbstractTreapSegmentSeq]], 
 * [[AbstractArraySegmentSeq]]).
 * 
 * Lazy evaluation is memory efficient for large sequences, since we don't create new data structure to store, 
 * for example, mapped sequence. Also it helps to avoid unnecessary computations, if we don't need the whole mapped
 * sequence, but just a small region of it. On the other hand, long chain of lazy operations (i.e when we apply `map`, 
 * `zip`, etc. many times in a row) can cause performance penalty, especially when one needs to repeatedly request same 
 * segment or value. If necessary, each lazy sequence can be converted to equivalent strict sequence with
 * [[SegmentSeqT.strict]] method.
 * 
 * @tparam E type of elements on ordered domain
 * @tparam D type of ordered domain
 * @tparam V type of value assigned to range of elements
 * @tparam S type of range state
 */
trait StrictSegmentSeqT[E, D[X] <: Domain[X], V, +S] extends SegmentSeqT[E, D, V, S] {

  final override def strict: StrictSegmentSeq[E, D, V] = this
}