import scala.annotation.tailrec
import scala.math.*

/** オブジェクトAKSPrimalityTestは、AKS素数判定法を用いて数が素数かどうかを判定するメソッドを提供します。
  */
object AKSPrimalityTest {

  /** ユークリッドの互除法を使用して2つの数の最大公約数（GCD）を計算します。
    *
    * @param a
    *   最初の数
    *
    * @param b
    *   2番目の数
    *
    * @return
    *   aとbの最大公約数
    */
  @tailrec
  def gcd(a: Long, b: Long): Long = {
    if (b == 0) {
      a
    } else {
      gcd(b, a % b)
    }
  }

  /** 与えられた数が他の数の冪であるかどうかを判定します。
    *
    * @param n
    *   判定対象の数
    *
    * @return
    *   nが他の数の冪である場合はtrue、それ以外の場合はfalse
    */
  def isPower(n: Long): Boolean = {
    (2 to (log(n) / log(2)).toInt).exists { b =>
      val a = pow(n, 1.0 / b).toLong
      pow(a, b) == n
    }
  }

  /** 拡張ユークリッドの互除法を使用して、最大公約数とその整数解（x, y）を計算します。
    *
    * @param a
    *   最初の数
    *
    * @param b
    *   2番目の数
    *
    * @return
    *   (gcd(a, b), x, y)、ただし ax + by = gcd(a, b)
    */
  def extendedGCD(a: Long, b: Long): (Long, Long, Long) = {
    if (b == 0) {
      (a, 1, 0)
    } else {
      val (gcd, x1, y1) = extendedGCD(b, a % b)
      (gcd, y1, x1 - (a / b) * y1)
    }
  }

  /** AKS素数判定法を使用して、与えられた数が素数かどうかを判定します。
    *
    * @param n
    *   判定対象の数
    *
    * @return
    *   nが素数である場合はtrue、それ以外の場合はfalse
    */
  def aksPrimalityTest(n: Long): Boolean = {
    if (n <= 1) {
      false
    } else if (n == 2) {
      true
    } else if (isPower(n)) {
      false
    } else {
      val log2n = log(n) / log(2)
      val r = LazyList
        .from(2)
        .find { r =>
          gcd(n, r) == 1 && order(n, r) > log2n * log2n
        }
        .get

      val maxA = (sqrt(phi(r)).toLong * log2n).toLong
      (1L to maxA).forall { a =>
        modPow(a, n, n) == a
      }
    }
  }

  /** 基数の累乗を法を用いて計算します。
    *
    * @param base
    *   基数
    *
    * @param exp
    *   指数
    *
    * @param mod
    *   法
    *
    * @return
    *   (base&#94;exp) % mod の値
    */
  def modPow(base: Long, exp: Long, mod: Long): Long = {
    @tailrec
    def loop(base: Long, exp: Long, mod: Long, result: Long): Long = {
      if (exp == 0) {
        result
      } else if ((exp & 1) == 1) {
        loop((base * base) % mod, exp >> 1, mod, (result * base) % mod)
      } else {
        loop((base * base) % mod, exp >> 1, mod, result)
      }
    }

    loop(base % mod, exp, mod, 1L)
  }

  /** nのrに対する位数を計算します。
    *
    * @param n
    *   基数
    *
    * @param r
    *   法
    *
    * @return
    *   nのrに対する位数
    */
  private def order(n: Long, r: Long): Long = {
    @tailrec
    def loop(k: Long, mod: Long): Long = {
      if (mod == 1) {
        k
      } else {
        loop(k + 1, (mod * (n % r)) % r)
      }
    }

    loop(1, n % r)
  }

  /** 与えられた数のオイラーのトーシェント関数の値を計算します。
    *
    * @param n
    *   計算対象の数
    *
    * @return
    *   nのオイラーのトーシェント関数の値
    */
  private def phi(n: Long): Long = {
    @tailrec
    def loop(m: Long, p: Long, result: Long): Long = {
      if (p * p > m) {
        if (m > 1) {
          result - result / m
        } else {
          result
        }
      } else if (m % p == 0) {
        loop(
          LazyList.iterate(m)(_ / p).dropWhile(_ % p == 0).head,
          p + 1,
          result - result / p
        )
      } else {
        loop(m, p + 1, result)
      }
    }

    loop(n, 2, n)
  }
}
