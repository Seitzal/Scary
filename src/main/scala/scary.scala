package object scary {
  
  def stringToByteArray(str : String) = str.getBytes("utf-8")

  def byteArrayToString(arr : Array[Byte]) = arr.map(_.toChar).mkString
  
  def unsign(b : Byte) : Int =
    if (b < 0) 256 + b else b

  def byteToHexString(b : Byte) : String = {
    val unsigned = unsign(b)
    (unsigned / 16).toHexString + (unsigned % 16).toHexString
  }

}