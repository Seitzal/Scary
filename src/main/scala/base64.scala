package scary

import scala.collection.mutable.ArrayBuffer

package object base64 {

  class MalformedStringException extends Exception("Base64: Malformed String")

  val table = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')).toArray :+ '+' :+ '/'

  def encode(data : Array[Byte]) : String = {
    val ndata = data.map(unsign)
    val mod3 = data.length % 3
    val regular = (for (i <- 0 until ndata.length - mod3 by 3) yield {
      val first = ndata(i) >> 2
      val second = ((ndata(i) & 3) << 4) | (ndata(i + 1) >> 4)
      val third = ((ndata(i + 1) & 15) << 2) | (ndata(i + 2) >> 6)
      val fourth = ndata(i + 2) & 63
      "" + table(first) + table(second) + table(third) + table(fourth)
    }).mkString
    if (mod3 == 2) {
      val first = ndata(ndata.length - 2) >> 2
      val second = ((ndata(ndata.length - 2) & 3) << 4) | (ndata(ndata.length - 1) >> 4)
      val third = (ndata(ndata.length - 1) & 15) << 2
      regular + table(first) + table(second) + table(third) + "="
    } else if (mod3 == 1) {
      val first = ndata(ndata.length - mod3) >> 2
      val second = (ndata(ndata.length - mod3) & 3) << 4
      regular + table(first) + table(second) + "=="
    } else regular
  }

  def decode(string : String) : Array[Byte] = {
    if (string.length % 4 != 0) throw new MalformedStringException
    val bytes = new ArrayBuffer[Byte]()
    for (i <- 0 until string.length by 4) {
      val first = (table.indexOf(string.charAt(i)) << 2) | 
        ((table.indexOf(string.charAt(i + 1)) & 48) >> 4)
      bytes += first.toByte
      if (string.charAt(i + 2) != '=') {
        val second = ((table.indexOf(string.charAt(i + 1)) & 15) << 4) | 
          ((table.indexOf(string.charAt(i + 2)) & 60) >> 2)
        bytes += second.toByte
        if (string.charAt(i + 3) != '=') {
          val third = ((table.indexOf(string.charAt(i + 2)) & 3) << 6) | 
            table.indexOf(string.charAt(i + 3))
          bytes += third.toByte
        }
      }
    }
    bytes.toArray
  }
}