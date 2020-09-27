package ordset.util

package object types {

  type >|<[-U, V] = Dual[U] { type Out = V }
}
