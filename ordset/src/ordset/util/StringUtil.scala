package ordset.util

protected[ordset] object StringUtil {

  private val etcString: String = "..."
  
  /**
   * Replaces all occurrences of [[System.lineSeparator]] in input string with empty string.
   */
  def collapse(str: String): String = str.replace(System.lineSeparator, "").nn

  /**
   * Takes first `len` characters from the input string. If some characters were dropped, then `...` string is
   * appended to the end. Length of output string can't be greater than specified `len`.
   */
  def limit(str: String, len: Int): String = 
    if (len <= 0 || str.length <= len) str
    else {
      if (etcString.length >= len) {
        etcString.substring(0, len).nn
      } else {
        str.substring(0, len - etcString.length).nn + etcString
      }
    }
}
