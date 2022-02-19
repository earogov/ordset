package ordset.core.segmentSeq.validation

import scala.collection.{AbstractIterator, Iterator}
import ordset.core.segmentSeq.SegmentSeqException
import ordset.util.types.Undefined
import scala.collection.IterableOps

/**
 * Iterable with validation condition. Iterator can perform validation of its current state on demand.
 */
trait ValidatingIterable[+E] extends Iterable[E] {

  override def iterator: ValidatingIterable.ValidatingIterator[E]

  /**
   * Original iterable, to which validation is applied.
   */
  def originalIterable: Iterable[E]

  /**
   * Returns `true`, if all elements of iterable satisfy validation condition.
   */
  def allValid: Boolean = iterator.allValid

  /**
   * Throws exception, if any element of iterable doesn't satisfy validation condition.
   */
  @throws[ValidationException]
  def validateAll(): Unit = iterator.validateAll()

  /**
   * Folds iterable with function `f` and initial state `init` going left to right. Validation is applied before 
   * each call of function `f`.
   */
  @throws[ValidationException]
  def foldLeftValidated[R](init: R)(f: (R, E) => R): R = iterator.foldLeftValidated(init)(f)
}

object ValidatingIterable {

  /**
   * Returns iterable without actual validation. It is always passed if requested.
   */
  def unchecked[E](iterable: Iterable[E]): ValidatingIterable[E] = new UncheckedIterable(iterable)

  /**
   * Returns empty iterable.
   */
  val empty: ValidatingIterable[Nothing] = new UncheckedIterable(List())

  /**
   * Base trait for iterators with validation.
   */
  trait ValidatingIterator[+E] extends Iterator[E] {

    /**
     * Returns `true`, if current state is valid. Current state may include one or several last elements, 
     * returned by iterator.
     */
    def isValid: Boolean

    /**
     * Throws exception, if current state is not valid. Current state may include one or several last elements, 
     * returned by iterator.
     */
    @throws[ValidationException]
    def validate(): Unit

    /**
     * Returns `true`, if all next elements satisfy validation condition.
     * 
     * Note, method contract does not enforce any requirements on iterator state after execution. It may be fully
     * exhausted or not. So iterator should not be used after method call.
     */
    def allValid: Boolean = {
      var isValid = true
      while (this.hasNext && isValid) {
        this.next()
        isValid = this.isValid
      }
      isValid
    }

    /**
     * Throws exception, if any next element doesn't satisfy validation condition.
     * 
     * Note, method contract does not enforce any requirements on iterator state after execution. It may be fully
     * exhausted or not. So iterator should not be used after method call.
     */
    @throws[ValidationException]
    def validateAll(): Unit = {
      val iterator = this.iterator
      while (this.hasNext) {
        this.next()
        this.validate()
      }
    }

    /**
     * Folds sequence of next elements with function `f` and initial state `init` going left to right.
     * Validation is applied before each call of function `f`.
     */
    @throws[ValidationException]
    def foldLeftValidated[R](init: R)(f: (R, E) => R): R = {
      var result = init
      while (this.hasNext) {
        val next = this.next()
        this.validate()
        result = f(result, next)
      }
      result
    }
  }

  /**
   * Iterator without actual validation. It is always passed if requested.
   */
  class UncheckedIterator[E](
    protected val originalIterator: Iterator[E]
  ) extends AbstractIterator[E] with ValidatingIterator[E] {

    @throws[NoSuchElementException]
    override def next(): E = originalIterator.next()

    override def hasNext: Boolean = originalIterator.hasNext

    override def isValid: Boolean = true

    @throws[ValidationException]
    override def validate(): Unit = {}

    override def allValid: Boolean = true

    @throws[ValidationException]
    override def validateAll(): Unit = {}

    @throws[ValidationException]
    override def foldLeftValidated[R](init: R)(f: (R, E) => R): R = originalIterator.foldLeft(init)(f)
  }

  /**
   * Iterable without actual validation. It is always passed if requested.
   */
  class UncheckedIterable[E](
    override val originalIterable: Iterable[E]
  ) extends ValidatingIterable[E] {

    override def iterator: ValidatingIterator[E] = new UncheckedIterator(originalIterable.iterator)

    override def allValid: Boolean = true

    override def validateAll(): Unit = {}
  }

  /**
   * Base class for iterators with validation.
   */
  abstract class AbstractValidatingIterator[E](
    protected val originalIterator: Iterator[E]
  ) extends AbstractIterator[E] with ValidatingIterator[E] {

    @throws[NoSuchElementException]
    override def next(): E = {
      val n = originalIterator.next()
      index = index + 1
      prevCache = nextCache
      nextCache = n
      n
    }

    override def hasNext: Boolean = originalIterator.hasNext

    // Protected section -------------------------------------------------------- //

    /** Previous element, that was returned by [[next()]] method. */
    protected var prevCache: Undefined[E] = Undefined

    /** Last element, that was returned by [[next()]] method. */
    protected var nextCache: Undefined[E] = Undefined

    /** Index of last element, that was returned by [[next()]] method. */
    protected var index: Long = -1
  }

  /**
   * Iterable, that applies validation to a single element.
   */
  class ValidatingIterableArity1[E](
    override val originalIterable: Iterable[E],
    protected val validation: ValidationPredicate.Arity1[E]
  ) extends ValidatingIterable[E] {

    override def iterator: ValidatingIterator[E] = new ValidatingIteratorArity1(originalIterable.iterator, validation)
  }

  /**
   * Iterator, that applies validation to a single element.
   * 
   * If current element is undefined ([[next()]] method was not called yet), then validation is also passed.
   */
  class ValidatingIteratorArity1[E](
    protected override val originalIterator: Iterator[E],
    protected val validation: ValidationPredicate.Arity1[E]
  ) extends AbstractValidatingIterator[E](originalIterator) {

    override def isValid: Boolean = nextCache match {
      case Undefined => true
      case c: E @unchecked => validation.apply(c)
    }

    @throws[ValidationException]
    override def validate(): Unit = nextCache match {
      case Undefined => {}
      case c: E @unchecked => validation.validate(c, index)
    }
  }

  /**
   * Iterable, that applies validation to a pair of adjacent elements.
   */
  class ValidatingIterableArity2[E](
    override val originalIterable: Iterable[E],
    protected val validation: ValidationPredicate.Arity2[E]
  ) extends ValidatingIterable[E] {

    override def iterator: ValidatingIterator[E] = new ValidatingIteratorArity2(originalIterable.iterator, validation)
  }

  /**
   * Iterator, that applies validation to a pair of adjacent elements.
   * 
   * If any element of a pair is undefined ([[next()]] method was not called yet, or was called only once), 
   * then validation is also passed.
   */
  class ValidatingIteratorArity2[E](
    protected override val originalIterator: Iterator[E],
    protected val validation: ValidationPredicate.Arity2[E]
  ) extends AbstractValidatingIterator[E](originalIterator) {

    override def isValid: Boolean = nextCache match {
      case Undefined => true
      case nc: E @unchecked => prevCache match {
        case Undefined => true
        case pc: E @unchecked => validation.apply(pc, nc)
      }
    }

    @throws[ValidationException]
    override def validate(): Unit = nextCache match {
      case Undefined => {}
      case nc: E @unchecked => prevCache match {
        case Undefined => {}
        case pc: E @unchecked => validation.validate(pc, nc, index)
      }
    }
  }

  /**
   * Iterable, combining [[ValidatingIterableArity1]] and [[ValidatingIterableArity2]].
   */
  class ValidatingIterableArity1And2[E](
    override val originalIterable: Iterable[E],
    protected val validation1: ValidationPredicate.Arity1[E],
    protected val validation2: ValidationPredicate.Arity2[E]
  ) extends ValidatingIterable[E] {

    override def iterator: ValidatingIterator[E] = 
      new ValidatingIteratorArity1And2(originalIterable.iterator, validation1, validation2)
  }

  /**
   * Iterator, combining [[ValidatingIterableArity1]] and [[ValidatingIterableArity2]].
   */
  class ValidatingIteratorArity1And2[E](
    protected override val originalIterator: Iterator[E],
    protected val validation1: ValidationPredicate.Arity1[E],
    protected val validation2: ValidationPredicate.Arity2[E]
  ) extends AbstractValidatingIterator[E](originalIterator) {

    override def isValid: Boolean = nextCache match {
      case Undefined => true
      case nc: E @unchecked => 
        if (validation1.apply(nc)) prevCache match {
          case Undefined => true
          case pc: E @unchecked => validation2.apply(pc, nc)
        }
        else false
    }

    @throws[ValidationException]
    override def validate(): Unit = nextCache match {
      case Undefined => {}
      case nc: E @unchecked => 
        validation1.validate(nc, index)
        prevCache match {
          case Undefined => {}
          case pc: E @unchecked => validation2.validate(pc, nc, index)
        }
    }
  }
}