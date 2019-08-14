def fixbyte(b : Byte) : Int =
  if (b < 0) 256 + b else b

println(fixbyte(254.toByte))