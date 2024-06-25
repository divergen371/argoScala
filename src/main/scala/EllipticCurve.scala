import java.security.{MessageDigest, SecureRandom}

/** 楕円曲線を表すクラス。
  *
  * @param a
  *   楕円曲線のパラメータa
  * @param b
  *   楕円曲線のパラメータb
  * @param p
  *   素数p（有限体のモジュラス）
  */
class EllipticCurve(a: BigInt, b: BigInt, p: BigInt) {

  /** 鍵ペアを生成。
    *
    * @param basePoint
    *   基点
    * @return
    *   秘密鍵と公開鍵のペア
    */
  def generateKeyPair(basePoint: Point): (BigInt, Point) = {
    val privateKey = generateRandomScalar()
    val publicKey = scalarMultiply(privateKey, basePoint)
    (privateKey, publicKey)
  }

  /** メッセージの暗号化。
    *
    * @param publicKey
    *   受信者の公開鍵
    * @param message
    *   暗号化するメッセージ（点として表現）
    * @return
    *   暗号化されたメッセージ（点のペア）
    */
  def encrypt(
      publicKey: Point,
      message: Point,
      basePoint: Point
  ): (Point, Point) = {
    val k = generateRandomScalar()
    val c1 = scalarMultiply(k, basePoint)
    val c2 = addPoints(message, scalarMultiply(k, publicKey))
    (c1, c2)
  }

  /** ランダムなスカラー値を生成。
    *
    * @return
    *   ランダムなスカラー値
    */
  def generateRandomScalar(): BigInt = {
    val random = new SecureRandom()
    BigInt.probablePrime(256, random).mod(p)
  }

  /** スカラー倍（点の乗算）を実装。
    *
    * @param k
    *   スカラー値
    * @param p
    *   楕円曲線上の点
    * @return
    *   スカラー倍された点
    */
  def scalarMultiply(k: BigInt, p: Point): Point = {
    var n = k
    var q = p
    var result = Point(0, 0) // 無限遠点

    while (n > 0) {
      if ((n & 1) == 1) {
        result = addPoints(result, q)
      }
      q = addPoints(q, q)
      n >>= 1
    }
    result
  }

  /** 2つの楕円曲線上の点を加算する。
    *
    * @param p1
    *   加算する最初の点
    * @param p2
    *   加算する2番目の点
    * @return
    *   加算結果の点
    */
  def addPoints(p1: Point, p2: Point): Point = {
    if (p1 == p2) {
      // p1とp2が同じ点の場合
      val m = (3 * p1.x.pow(2) + a) * (2 * p1.y).modInverse(p) % p
      val x3 = (m.pow(2) - 2 * p1.x) % p
      val y3 = (m * (p1.x - x3) - p1.y) % p
      Point(x3, y3)
    } else {
      // p1とp2が異なる点の場合
      val m = (p2.y - p1.y) * (p2.x - p1.x).modInverse(p) % p
      val x3 = (m.pow(2) - p1.x - p2.x) % p
      val y3 = (m * (p1.x - x3) - p1.y) % p
      Point(x3, y3)
    }
  }

  /** メッセージの復号。
    *
    * @param privateKey
    *   受信者の秘密鍵
    * @param ciphertext
    *   暗号化されたメッセージ（点のペア）
    * @return
    *   復号されたメッセージ（点）
    */
  def decrypt(privateKey: BigInt, ciphertext: (Point, Point)): Point = {
    val (c1, c2) = ciphertext
    val s = scalarMultiply(privateKey, c1)
    val message = addPoints(c2, Point(s.x, -s.y))
    message
  }

  /** メッセージの署名を生成。
    *
    * @param privateKey
    *   署名者の秘密鍵
    * @param message
    *   署名するメッセージ
    * @return
    *   署名（r, s）
    */
  def sign(
      privateKey: BigInt,
      message: Array[Byte],
      basePoint: Point
  ): (BigInt, BigInt) = {
    val random = new SecureRandom()
    val k = BigInt.probablePrime(256, random).mod(p)
    val r = scalarMultiply(k, basePoint).x.mod(p)
    val z = BigInt(MessageDigest.getInstance("SHA-256").digest(message)).mod(p)
    val s = ((z + r * privateKey) * k.modInverse(p)).mod(p)
    (r, s)
  }

  /** メッセージの署名を検証。
    *
    * @param publicKey
    *   署名者の公開鍵
    * @param message
    *   検証するメッセージ
    * @param signature
    *   検証する署名（r, s）
    * @return
    *   署名が正しければtrue、そうでなければfalse
    */
  def verify(
      publicKey: Point,
      message: Array[Byte],
      signature: (BigInt, BigInt),
      basePoint: Point
  ): Boolean = {
    val (r, s) = signature
    val z = BigInt(MessageDigest.getInstance("SHA-256").digest(message)).mod(p)
    val w = s.modInverse(p)
    val u1 = (z * w).mod(p)
    val u2 = (r * w).mod(p)
    val point =
      addPoints(scalarMultiply(u1, basePoint), scalarMultiply(u2, publicKey))
    r == point.x.mod(p)
  }
}
