package scary

import scala.util.Random

/**
 *  A simple stream cipher, whch XORs the plain data with a stream of
 *  pseudo-random bytes. Uses scala.util.Random, seeded with the 64-bit
 *  encryption key, to generate the pseudorandoms.
 */
package object ranstreamxor {

  def apply(key : Long, data : Array[Byte]) : Array[Byte] = {
    val ranstream = new Random(key)
    data.map(byte => (byte ^ ranstream.nextInt(256)).toByte)
  }

  def apply(key : Array[Byte], data : Array[Byte]) : Array[Byte] =
    apply(cleanKey(key), data)

  def apply_multipass(key : Array[Byte], data : Array[Byte]) : Array[Byte] = {
    val keys = for (i <- 0 until key.length by 8)
      yield key.slice(i, math.min(key.length, i + 8))
    def iter(keyIdx : Int, data : Array[Byte]) : Array[Byte] =
      if (keyIdx == keys.length) data
      else iter(keyIdx + 1, apply(keys(keyIdx), data))
    iter(0, data)
  }

  def apply_multipass(key : String, data : Array[Byte]) : Array[Byte] =
    apply_multipass(stringToByteArray(key), data)

  def apply_multipass(key : Array[Byte], data : String) : String =
    byteArrayToString(
      apply_multipass(
        key,
        stringToByteArray(data)))

  def apply_multipass(key : String, data : String) : String =
    byteArrayToString(
      apply_multipass(
        stringToByteArray(key),
        stringToByteArray(data)))

  private def cleanKey(key : Array[Byte]) : Long = {
    var acc = 0L
    for (i <- 0 until math.min(key.length, 8)) {
      acc = acc | (key(i) << 8 * (key.length - 1 - i))
    }
    acc
  }

}