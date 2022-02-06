package ordset.core.segmentSeq.validation

class ValidationException(message: String, cause: Throwable | Null) extends RuntimeException(message, cause)

object ValidationException {

  def apply(message: String, cause: Throwable): ValidationException = new ValidationException(message, cause)

  def apply(message: String): ValidationException = new ValidationException(message, null)

  def apply(cause: Throwable): ValidationException = new ValidationException("", cause)

  def invalidObject(objectStr: String, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid object $objectStr", causeStr)

  def invalidObjectsSeq(prevStr: String, nextStr: String, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid sequence of objects {$prevStr, $nextStr}", causeStr)

  def invalidBound(boundStr: String, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid bound $boundStr", causeStr)

  def invalidBoundsSeq(prevStr: String, nextStr: String, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid sequence of bounds {$prevStr, $nextStr}", causeStr)

  def invalidValue(valueStr: String, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid value $valueStr", causeStr)

  def invalidValuesSeq(prevStr: String, nextStr: String, causeStr: String = ""): ValidationException = 
    withMessageAndCause(s"Invalid sequence of values {$prevStr, $nextStr}", causeStr)

  // Private section ---------------------------------------------------------- //
  private def withMessageAndCause(msg: String, cause: String = ""): ValidationException =
    if !cause.isEmpty then ValidationException(s"$msg: $cause")
    else ValidationException(msg)
}
