package ordset.core.segmentSeq.validation

class ValidationException(message: String, cause: Throwable | Null) extends RuntimeException(message, cause)

object ValidationException {

  def apply(message: String, cause: Throwable): ValidationException = new ValidationException(message, cause)

  def apply(message: String): ValidationException = new ValidationException(message, null)

  def apply(cause: Throwable): ValidationException = new ValidationException("", cause)

  def invalidObject(objectStr: String, index: Long, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid object $objectStr", index, causeStr)

  def invalidObjectsSeq(prevStr: String, nextStr: String, index: Long, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid sequence of objects {$prevStr, $nextStr}", index, causeStr)

  def invalidBound(boundStr: String, index: Long, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid bound $boundStr", index, causeStr)

  def invalidBoundsSeq(prevStr: String, nextStr: String, index: Long, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid sequence of bounds {$prevStr, $nextStr}", index, causeStr)

  def invalidInterval(intervalStr: String, index: Long, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid interval $intervalStr", index, causeStr)

  def invalidIntervalsSeq(prevStr: String, nextStr: String, index: Long, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid sequence of intervals {$prevStr, $nextStr}", index, causeStr)

  def invalidValue(valueStr: String, index: Long, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid value $valueStr", index, causeStr)

  def invalidValuesSeq(prevStr: String, nextStr: String, index: Long, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid sequence of values {$prevStr, $nextStr}", index, causeStr)

  // Private section ---------------------------------------------------------- //
  private def withMessageAndCause(msg: String, index: Long, cause: String = ""): ValidationException = {
    val err = if cause.isEmpty then msg else s"$msg: $cause"
    return ValidationException(err + s". Index = $index")
  }
}
