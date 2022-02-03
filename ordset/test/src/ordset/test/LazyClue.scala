package ordset.test

@FunctionalInterface
trait LazyClue extends (() => String) {

  def apply(): String
  
  override def toString(): String = apply()
}