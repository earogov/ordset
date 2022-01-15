// package ordset.core.validation

// import ordset.Show
// import ordset.core.domain.Domain
// import ordset.core.range.Range
// /**
//  * Exception of elements validation.
//  */
// class InvalidElementException(message: String, cause: Throwable | Null) extends RuntimeException(message, cause)

// object InvalidElementException {

//   def apply(message: String, cause: Throwable): InvalidElementException = new InvalidElementException(message, cause)

//   def apply(message: String): InvalidElementException = new InvalidElementException(message, null)

//   def apply(cause: Throwable): InvalidElementException = new InvalidElementException("", cause)
  
//   def invalidElement[E](element: E, causeStr: String | Null)(implicit elementShow: Show[E]): InvalidElementException = {
//     var msg = s"Invalid element ${elementShow.show(element)}"
//     if (causeStr != null) msg = s"$msg: $causeStr"
//     InvalidElementException(msg)
//   }

//   def outOfDomainElement[E](element: E, domain: Domain[E])(implicit elementShow: Show[E]): InvalidElementException = {
//     val boundsStr = Range.defaultShow(elementShow).show(domain.)
//     invalidElement(elementStr, s"element doesn't belong to domain with bounds ${domain.]")
//   }
// }
