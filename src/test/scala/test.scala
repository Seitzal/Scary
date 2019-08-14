package scary.test

import scary._
import org.scalatest._
import java.util.Arrays

class ScaryTest extends FunSuite {
  test ("Base64 encode") {
    val plain = stringToByteArray("Red")
    assert(base64.encode(plain).equals("UmVk"))
  }

  test ("Base64 decode") {
    val code = "UmVk"
    assert(byteArrayToString(base64.decode(code)).equals("Red"))
  }

  test ("Random Stream XOR") {
    val plain = stringToByteArray("Super secret message nobody may read")
    val key = stringToByteArray("password")
    val cipher = ranstreamxor.apply(key, plain)
    assert(Arrays.equals(ranstreamxor.apply(key, cipher), plain))
  }

  test("Random Stream XOR, overlong key") {
    val plain = stringToByteArray("Short message")
    val key = stringToByteArray("A rather long key string")
    val cipher = ranstreamxor.apply(key, plain)
    assert(Arrays.equals(ranstreamxor.apply(key, cipher), plain))
  }

  test("Multipass random stream XOR") {
    val plain = stringToByteArray("Super secret message nobody may read")
    val key = stringToByteArray("A rather long key string")
    val cipher = ranstreamxor.apply_multipass(key, plain)
    assert(Arrays.equals(ranstreamxor.apply_multipass(key, cipher), plain))
  }

  test("Keygen") {
    val key = keygen.newKey(32)
    assert(Arrays.equals(key, base64.decode(base64.encode(key))))
  }

  test("Multipass random stream XOR with random key, short text") {
    val plain = stringToByteArray("Super secret message nobody may read")
    val key = keygen.newKey(32)
    val cipher = ranstreamxor.apply_multipass(key, plain)
    assert(Arrays.equals(ranstreamxor.apply_multipass(key, cipher), plain)) 
  }

  test("Multipass random stream XOR with random key, long text") {
    val plain = 
      stringToByteArray(scala.io.Source.fromFile("resolution.txt").mkString)
    val key = keygen.newKey(32)
    val cipher = ranstreamxor.apply_multipass(key, plain)
    assert(Arrays.equals(ranstreamxor.apply_multipass(key, cipher), plain)) 
  }

  test("AES keyExpand") {
    val key = 
      Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15).map(_.toByte)
    val round_keys = aes.keyExpand128(key)
    assert(round_keys.flatten.length == 176)
  }

  test("AES addRoundKey") {
    val state = (200 to 215).map(_.toByte).toArray
    val key = keygen.newKey(16)
    aes.addRoundKey(state, key)
    aes.addRoundKey(state, key)
    assert(Arrays.equals((200 to 215).map(_.toByte).toArray, state))
  }

  test("AES subBytes") {
    val state = (200 to 215).map(_.toByte).toArray
    val expected = aes.S_BOX.drop(200).take(16)
    aes.subBytes(state)
    assert(Arrays.equals(state, expected))
  }

  test("AES shiftRows") {
    val state = (0 until 16).map(_.toByte).toArray
    val expected = 
      Array(0, 5, 10, 15, 4, 9, 14, 3, 8, 13, 2, 7, 12, 1, 6, 11).map(_.toByte)
    aes.shiftRows(state)
    assert(Arrays.equals(state, expected))
  }

  test("AES mixColumns") {
    val state = 
      Array(219,  19,  83,  69, 
            242,  10,  34,  92, 
            212, 212, 212, 213,
             45,  38,  49,  76
      ).map(_.toByte)
    val expected = 
      Array(142,  77, 161, 188,
            159, 220,  88, 157,
            213, 213, 215, 214,
             77, 126, 189, 248
      ).map(_.toByte)
    aes.mixColumns(state)
    assert(Arrays.equals(state, expected))
  }
  
  
  test("AES encrypt: 128-bit key; single block") {
    val plain = stringToByteArray("This is a messag")
    assert(plain.length == 16)
    val key = (1 to 16).toArray.map(_.toByte)
    val cipher = aes.encryptBlock128(plain, aes.keyExpand128(key))
    assert(cipher.length == 16)
    val expected = Array(
      0xb6, 0x4b, 0x27, 0xbb, 0x16, 0x15, 0xA6, 0xf5, 
      0x32, 0x18, 0x6c, 0xc5, 0xfa, 0x94, 0xb5, 0x5e
    ).map(_.toByte)
    assert(Arrays.equals(cipher, expected))
  }

  test("AES encrypt: 128-bit key; short message") {
    val plain = stringToByteArray("This is a message we will encrypt with AES!")
    val key = (1 to 16).toArray.map(_.toByte)
    val cipher = aes.encrypt128(plain, key)
    val expected = base64.decode(
      "tksnuxYVpvUyGGzF+pS1XlxU6hvflx494xv8AnUidlLVe9VCug9oUM39WbjrDoPR")
    assert(Arrays.equals(cipher, expected))
  }
  
  test("AES encrypt, 128-bit key; long message") {
    val plain = stringToByteArray(
      scala.io.Source.fromFile("resolution.txt").mkString)
    val key = (1 to 16).toArray.map(_.toByte)
    val cipher = aes.encrypt128(plain, key)
    assert(cipher.length == plain.length + 16 - plain.length % 16)
  }
}
