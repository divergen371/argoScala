/** ECCTestオブジェクトは、楕円曲線暗号の基本操作を実演する。
  */
object ECCTest {

  /** プログラムのエントリポイント。楕円曲線上の点の加算、スカラー倍、鍵生成、 メッセージの暗号化と復号、署名と検証のデモンストレーションを行う。
    *
    * @param args
    *   コマンドライン引数（使用しない）
    */
  def main(args: Array[String]): Unit = {
    // 楕円曲線パラメータ (secp256k1: y^2 = x^3 + 7)
    val a = BigInt("0")
    val b = BigInt("7")
    val p = BigInt(
      "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F",
      16
    )
    val basePoint = Point(
      BigInt(
        "6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296",
        16
      ),
      BigInt(
        "4fe342e2fe1a7f9b8ee7eb4a7c0f9e162cbddce1009244c8f911c7c2b1c5994f",
        16
      )
    )
    // 鍵ペアを生成
    val (privateKey, publicKey) = EllipticCurve.generateKeyPair(basePoint, a, p)

    println(s"Private Key: $privateKey")
    println(s"Public Key: (${publicKey.x}, ${publicKey.y})")

    // メッセージの暗号化と復号
    val message = Point(BigInt("12345"), BigInt("67890"))
    val ciphertext = EllipticCurve.encrypt(publicKey, message, basePoint, a, p)
    println(
      s"Ciphertext: (${ciphertext._1.x}, ${ciphertext._1.y}), (${ciphertext._2.x}, ${ciphertext._2.y})"
    )

    val decryptedMessage = EllipticCurve.decrypt(privateKey, ciphertext, a, p)
    println(
      s"Decrypted Message: (${decryptedMessage.x}, ${decryptedMessage.y})"
    )

    // メッセージの署名と検証
    val signMessage = "Hello, ECC!".getBytes("UTF-8")
    val signature = EllipticCurve
      .sign(privateKey, signMessage, basePoint, a, p)
    println(s"Signature: (r: ${signature._1}, s: ${signature._2})")

    val isValid = EllipticCurve
      .verify(publicKey, signMessage, signature, basePoint, a, p)
    println(s"Signature valid: $isValid")
  }
}
