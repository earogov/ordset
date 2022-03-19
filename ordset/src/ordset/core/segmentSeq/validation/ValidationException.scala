package ordset.core.segmentSeq.validation

import ordset.util.StringUtil

class ValidationException(message: String, cause: Throwable | Null) extends RuntimeException(message, cause)

object ValidationException {

  def apply(message: String, cause: Throwable): ValidationException = new ValidationException(message, cause)

  def apply(message: String): ValidationException = new ValidationException(message, null)

  def apply(cause: Throwable): ValidationException = new ValidationException("", cause)

  def invalidObject(objectStr: String, index: Long, causeStr: String = ""): ValidationException = {
    val obj = limitObjectStr(objectStr)
    withMessageAndCause(s"Invalid object $obj", index, causeStr)
  }

  def invalidObjectsSeq(prevStr: String, nextStr: String, index: Long, causeStr: String = ""): ValidationException = {
    val prev = limitObjectStr(prevStr)
    val next = limitObjectStr(nextStr)
    withMessageAndCause(s"Invalid sequence of objects {$prev, $next}", index, causeStr)
  }

  def invalidBound(boundStr: String, index: Long, causeStr: String = ""): ValidationException = {
    val bound = limitObjectStr(boundStr)
    withMessageAndCause(s"Invalid bound $bound", index, causeStr)
  }

  def invalidBoundsSeq(prevStr: String, nextStr: String, index: Long, causeStr: String = ""): ValidationException = {
    val prev = limitObjectStr(prevStr)
    val next = limitObjectStr(nextStr)
    withMessageAndCause(s"Invalid sequence of bounds {$prev, $next}", index, causeStr)
  }

  def invalidInterval(intervalStr: String, index: Long, causeStr: String = ""): ValidationException = {
    val interval = limitObjectStr(intervalStr)
    withMessageAndCause(s"Invalid interval $interval", index, causeStr)
  }

  def invalidIntervalsSeq(prevStr: String, nextStr: String, index: Long, causeStr: String = ""): ValidationException = {
    val prev = limitObjectStr(prevStr)
    val next = limitObjectStr(nextStr)
    withMessageAndCause(s"Invalid sequence of intervals {$prev, $next}", index, causeStr)
  }

  def invalidIntervalRel(relStr: String, index: Long, causeStr: String = ""): ValidationException = {
    val rel = limitObjectStr(relStr)
    withMessageAndCause(s"Invalid interval relation $rel", index, causeStr)
  }

  def invalidIntervalRelSeq(
    prevStr: String, 
    nextStr: String, 
    index: Long, 
    causeStr: String = ""
  ): ValidationException = {
    val prev = limitObjectStr(prevStr)
    val next = limitObjectStr(nextStr)
    withMessageAndCause(s"Invalid sequence of interval relations {$prev, $next}", index, causeStr)
  }

  def invalidValue(valueStr: String, index: Long, causeStr: String = ""): ValidationException = {
    val value = limitObjectStr(valueStr)
    withMessageAndCause(s"Invalid value $value", index, causeStr)
  }

  def invalidValuesSeq(prevStr: String, nextStr: String, index: Long, causeStr: String = ""): ValidationException = {
    val prev = limitObjectStr(prevStr)
    val next = limitObjectStr(nextStr)
    withMessageAndCause(s"Invalid sequence of values {$prev, $next}", index, causeStr)
  }

  // Private section ---------------------------------------------------------- //
  private val maxObjectLen = 256

  private def limitObjectStr(str: String): String = StringUtil.limit(str, maxObjectLen)

  private def withMessageAndCause(msg: String, index: Long, cause: String = ""): ValidationException = {
    val err = if cause.isEmpty then msg else s"$msg: $cause"
    return ValidationException(err + s". Index = $index")
  }
}
