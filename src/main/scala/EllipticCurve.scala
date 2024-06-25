import java.security.{MessageDigest, SecureRandom}

object EllipticCurve {
  private val random: SecureRandom = new SecureRandom()

  /** 鍵ペアを生成。
    *
    * @param basePoint
    *   基点
    * @param a
    *   楕円曲線のパラメータa
    * @param p
    *   素数p（有限体のモジュラス）
    * @return
    *   秘密鍵と公開鍵のペア
    */
  def generateKeyPair(
      basePoint: Point,
      a: BigInt,
      p: BigInt
  ): (BigInt, Point) = {
    val privateKey: BigInt = generateRandomScalar(p)
    val publicKey: Point = scalarMultiply(privateKey, basePoint, a, p)
    (privateKey, publicKey)
  }

  /** メッセージの暗号化。
    *
    * @param publicKey
    *   受信者の公開鍵
    * @param message
    *   暗号化するメッセージ（点として表現）
    * @param basePoint
    *   基点
    * @param a
    *   楕円曲線のパラメータa
    * @param p
    *   素数p（有限体のモジュラス）
    * @return
    *   暗号化されたメッセージ（点のペア）
    */
  def encrypt(
      publicKey: Point,
      message: Point,
      basePoint: Point,
      a: BigInt,
      p: BigInt
  ): (Point, Point) = {
    val k: BigInt = generateRandomScalar(p)
    val c1: Point = scalarMultiply(k, basePoint, a, p)
    val c2: Point = addPoints(message, scalarMultiply(k, publicKey, a, p), a, p)
    (c1, c2)
  }

  /** メッセージの復号。
    *
    * @param privateKey
    *   受信者の秘密鍵
    *
    * @param ciphertext
    *   暗号化されたメッセージ（点のペア）
    *
    * @param a
    *   楕円曲線のパラメータa
    *
    * @param p
    *   素数p（有限体のモジュラス）
    *
    * @return
    *   復号されたメッセージ（点）
    */
  def decrypt(
      privateKey: BigInt,
      ciphertext: (Point, Point),
      a: BigInt,
      p: BigInt
  ): Point = {
    val (c1, c2) = ciphertext
    val s = scalarMultiply(privateKey, c1, a, p)
    val sInverse = Point(s.x, p - s.y) // s.y の符号を反転
    addPoints(c2, sInverse, a, p)
  }

  /** メッセージの署名を生成。
    *
    * @param privateKey
    *   署名者の秘密鍵
    * @param message
    *   署名するメッセージ
    * @param basePoint
    *   基点
    * @param a
    *   楕円曲線のパラメータa
    * @param p
    *   素数p（有限体のモジュラス）
    * @return
    *   署名（r, s）
    */
  def sign(
      privateKey: BigInt,
      message: Array[Byte],
      basePoint: Point,
      a: BigInt,
      p: BigInt
  ): (BigInt, BigInt) = {
    val k: BigInt = generateRandomScalar(p)
    val r: BigInt = scalarMultiply(k, basePoint, a, p).x.mod(p)
    val z: BigInt =
      BigInt(1, MessageDigest.getInstance("SHA-256").digest(message)).mod(p)
    val s: BigInt = (k.modInverse(p) * (z + r * privateKey)).mod(p)
    (r, s)
  }

  /** ランダムなスカラー値を生成。
    *
    * @param p
    *   素数p（有限体のモジュラス）
    * @return
    *   ランダムなスカラー値
    */
  def generateRandomScalar(p: BigInt): BigInt = {
    BigInt(p.bitLength, random).mod(p - 1) + 1
  }

  /** メッセージの署名を検証。
    *
    * @param publicKey
    *   署名者の公開鍵
    * @param message
    *   検証するメッセージ
    * @param signature
    *   検証する署名（r, s）
    * @param basePoint
    *   基点
    * @param a
    *   楕円曲線のパラメータa
    * @param p
    *   素数p（有限体のモジュラス）
    * @return
    *   署名が正しければtrue、そうでなければfalse
    */
  def verify(
      publicKey: Point,
      message: Array[Byte],
      signature: (BigInt, BigInt),
      basePoint: Point,
      a: BigInt,
      p: BigInt
  ): Boolean = {
    val (r, s): (BigInt, BigInt) = signature
    if (r <= 0 || r >= p || s <= 0 || s >= p) return false
    val z: BigInt =
      BigInt(1, MessageDigest.getInstance("SHA-256").digest(message)).mod(p)
    val w: BigInt = s.modInverse(p)
    val u1: BigInt = (z * w).mod(p)
    val u2: BigInt = (r * w).mod(p)
    val point: Point = addPoints(
      scalarMultiply(u1, basePoint, a, p),
      scalarMultiply(u2, publicKey, a, p),
      a,
      p
    )
    r == point.x.mod(p)
  }

  /** スカラー倍（点の乗算）を実装。
    *
    * @param k
    *   スカラー値
    * @param point
    *   楕円曲線上の点
    * @param a
    *   楕円曲線のパラメータa
    * @param p
    *   素数p（有限体のモジュラス）
    * @return
    *   スカラー倍された点
    */
  def scalarMultiply(k: BigInt, point: Point, a: BigInt, p: BigInt): Point = {
    @scala.annotation.tailrec
    def loop(n: BigInt, q: Point, acc: Point): Point = {
      if (n == 0) acc
      else if ((n & 1) == 1)
        loop(n >> 1, addPoints(q, q, a, p), addPoints(acc, q, a, p))
      else loop(n >> 1, addPoints(q, q, a, p), acc)
    }
    loop(k, point, Point(0, 0))
  }

  /** 2つの楕円曲線上の点を加算する。
    *
    * @param p1
    *   加算する最初の点
    * @param p2
    *   加算する2番目の点
    * @param a
    *   楕円曲線のパラメータa
    * @param p
    *   素数p（有限体のモジュラス）
    * @return
    *   加算結果の点
    */
  def addPoints(p1: Point, p2: Point, a: BigInt, p: BigInt): Point = {
    if (p1 == p2) {
      val m: BigInt = (3 * p1.x.pow(2) + a) * (2 * p1.y).modInverse(p) % p
      val x3: BigInt = (m.pow(2) - 2 * p1.x) % p
      val y3: BigInt = (m * (p1.x - x3) - p1.y) % p
      Point(x3, y3)
    } else {
      val m: BigInt = (p2.y - p1.y) * (p2.x - p1.x).modInverse(p) % p
      val x3: BigInt = (m.pow(2) - p1.x - p2.x) % p
      val y3: BigInt = (m * (p1.x - x3) - p1.y) % p
      Point(x3, y3)
    }
  }
}
