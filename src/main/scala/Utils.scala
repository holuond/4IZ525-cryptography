import scala.annotation.tailrec
import scala.math.BigInt

object Utils {
  def sqrt(number: BigInt): BigInt = {
    // guillaumeguy's implementation of BigInt sqrt from [[https://github.com/scala/bug/issues/3739]]
    def next(n: BigInt, i: BigInt): BigInt = (n + i / n) >> 1

    val one = BigInt(1)
    val n = one
    val n1 = next(n, number)

    @tailrec
    def sqrtHelper(n: BigInt, n1: BigInt): BigInt = if ((n1 - n).abs <= one) List(n1, n).max else sqrtHelper(n1, next(n1, number))

    sqrtHelper(n, n1)
  }

  def factorize(x: Int): List[Int] = {
    // from @LRLucena in https://stackoverflow.com/questions/30280524/scala-way-to-generate-prime-factors-of-a-number
    @tailrec
    def foo(x: Int, a: Int = 2, list: List[Int] = Nil): List[Int] = a*a > x match {
      case false if x % a == 0 => foo(x / a, a    , a :: list)
      case false               => foo(x    , a + 1, list)
      case true                => x :: list
    }
    foo(x)
  }
}