package ordset.util

package object types {
  
  protected[ordset] type Undefined[+X] = X | Undefined.type
}
