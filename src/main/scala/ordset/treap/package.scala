package ordset

import scala.collection.immutable

package object treap {

  //type TraverserState[K] = immutable.List[(Treap.Node[K, _], TraverserVisit.Type)]
  //type Traverser[K] = TraverserState[K] => TraverserResult.Type
  //type Reducer[K, R] = (TraverserState[K], TraverserResult.Type, R) => R

  type Traverser[K] = (Treap.Node[K, _], TraverserVisit.Type) => TraverserResult.Type
  type Reducer[K, R] = (Treap.Node[K, _], TraverserVisit.Type, TraverserResult.Type, R) => R
}
