package scary

import java.security.SecureRandom

object keygen {

  val seedsrc = "https://www.random.org/integers/" +
    "?num=1" +
    "&format=plain" +
    "&min=-1000000000" +
    "&max=1000000000" +
    "&col=1" +
    "&base=10"

  val ran = new java.security.SecureRandom
  reseed()


  /**
   * Attempts to reseed the key generator using random.org. If access is
   * unavailable for some reason, reseeds the generator using another
   * PRNG.
   */
  def reseed() : Unit = {
    try {
      val r = requests.get(seedsrc)
      if (r.statusCode == 200) {
        ran.setSeed(r.text.replace("\n", "").replace("\r", "").toLong)
      } else backup()
    } catch {
      case _ : Throwable => backup()
    }
    def backup() : Unit = {
      val helpran = new java.security.SecureRandom
      val seed = new Array[Byte](8)
      helpran.nextBytes(seed)
      ran.setSeed(seed)
    }
  }

  /**
   * Generates a random key for symmetric encryptions.
   * This uses the java.security.SecureRandom PRNG. If internet access is
   * available, it will be seeded using a true-random seed obtained from
   * random.org. Otherwise, seeding will be left to the JVM.
   * @param length The length of the key in bytes.
   */
  def newKey(length : Int) : Array[Byte] = {
    val response = new Array[Byte](length)
    ran.nextBytes(response)
    response
  }
}