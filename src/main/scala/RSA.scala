import scala.math.BigInt
import scala.math.BigInt.int2bigInt

/**
 * A very simple example of an RSA system.
 */
object RSA {
  def main(args: Array[String]): Unit = {
    generateCipher()
    return
  }

  /**
   * Generates an RSA cipher based on provided input:
   * generateCipher() generates a random RSA cipher
   * generateCipher(p, q) tries to generate an RSA cipher based on p, q
   * generateCipher(p, q, e) tries to generate an RSA cipher based on p, q, e
   */
  def generateCipher(_p: Option[BigInt] = None, _q: Option[BigInt] = None, _e: Option[BigInt] = None, primeBitLength: Int = 2048): RSACipher = {
    require(primeBitLength > 1)

    // Two different random prime numbers p, q
    val p = if (_p.isDefined) _p.get else BigInt.probablePrime(primeBitLength, scala.util.Random)
    var qCandidate = if (_q.isDefined) _q.get else BigInt.probablePrime(primeBitLength, scala.util.Random)
    while (qCandidate == p) {
      qCandidate = BigInt.probablePrime(primeBitLength, scala.util.Random)
    }
    val q = qCandidate
    println(s"p = $p")
    println(s"q = $q")

    // Modulus m and Euler's totient function's value for m
    val m = p * q
    val phiM: BigInt = (p - BigInt(1)) * (q - BigInt(1))
    println(s"m = $m")
    println(s"phi(m) = $phiM\n")

    // Public exponent e coprime with phi(m)
    val e = if (_e.isDefined) {
      _e.get
    } else {
      var eCandidate = BigInt(3)
      while (eCandidate.gcd(phiM) > 1) {
        eCandidate += 2
      }
      eCandidate
    }
    require(e.gcd(phiM) == 1)
    println(s"Public key: ($e, $m)")

    // Public exponent d as the inverse of e mod phiM
    val d = e.modInverse(phiM)
    require((e * d).mod(phiM) == 1)
    println(s"Private key: $d\n")

    // Encrypt a message using the public key and decrypt it with the private key
    val message = if (m <= Int.MaxValue)
      BigInt(scala.util.Random.nextInt(m.toInt)) else BigInt(scala.util.Random.nextInt(Int.MaxValue))
    val decryptedMessage = decrypt(encrypt(message, e, m), d, m)
    assert(message == decryptedMessage)

    RSACipher(p, q, m, phiM, e, d)
  }

  /**
   * Encrypts a message with a public key.
   *
   * @return ciphertext
   */
  def encrypt(message: BigInt, e: BigInt, m: BigInt): BigInt = {
    message.modPow(e, m)
  }

  /**
   * Decrypts a ciphertext with private key.
   *
   * @return plaintext
   */
  def decrypt(cipher: BigInt, d: BigInt, m: BigInt): BigInt = {
    cipher.modPow(d, m)
  }

  /**
   * Container for an RSA system.
   */
  case class RSACipher(p: BigInt,
                       q: BigInt,
                       m: BigInt,
                       phiM: BigInt,
                       e: BigInt,
                       d: BigInt)

}
