package ordset.util

private[ordset] object IterableUtil {

  /**
   * Returns next element if iterator. If there is no such element throws exception with specified message.
   */
  @throws[NoSuchElementException]
  def nextOrThrowMsg[A](i: Iterator[A], msg: String): A = {
    if (i.hasNext) i.next()
    else throw new NoSuchElementException(msg)
  }
}
