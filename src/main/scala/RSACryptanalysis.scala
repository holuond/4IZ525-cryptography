import RSA.generateCipher
import Utils.sqrt
import scala.math.BigInt
import scala.math.BigInt.int2bigInt

/**
 * This script shows a factorization attack on a weak RSA system (having a short public key).
 */
object RSACryptanalysis {
  def main(args: Array[String]): Unit = {
    val cipher = generateCipher(primeBitLength = 8)

    // Short public key => try to find p, q from m
    var pCandidate: BigInt = sqrt(cipher.m)
    if (pCandidate % 2 == 0) pCandidate += 1
    while (cipher.m % pCandidate > 0) {
      pCandidate -= 2
    }
    val p = pCandidate
    val q = cipher.m / p

    val brokenCipher = generateCipher(Some(p), Some(q), Some(cipher.e))
    assert(brokenCipher.d == cipher.d)
  }
}