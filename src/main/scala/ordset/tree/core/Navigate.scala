package ordset.tree.core

object Navigate {

  trait Func[K, V, Tree[KK, VV], -C, +S] extends ((Tree[K, V], C) => S)

  type BinaryFunc[K, V, Tree[KK, VV],  -C] = Func[K, V, Tree, C, BinaryTreeStep.Type]

  type GenericFunc[K, V, Tree[KK, VV],  -C] = Func[K, V, Tree, C, Any]
}
