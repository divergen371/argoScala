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

    // 楕円曲線を定義
    val curve = new EllipticCurve(a, b, p)
    val basePoint = Point(
      BigInt(
        "55066263022277343669578718895168534326250603453777594175500187360389116729240"
      ),
      BigInt(
        "32670510020758816978083085130507043184471273380659243275938904335757337482424"
      )
    )

    // ランダムなスカラー値を秘密鍵とし、それを基点にスカラー倍を行って公開鍵を生成
    val (privateKey, publicKey) = curve.generateKeyPair(basePoint)

    println(s"Private Key: $privateKey")
    println(s"Public Key: (${publicKey.x}, ${publicKey.y})")

    // メッセージの暗号化と復号

    val message = Point(BigInt("12345"), BigInt("67890"))
    val ciphertext = curve.encrypt(publicKey, message, basePoint)
    println(
      s"Ciphertext: (${ciphertext._1.x}, ${ciphertext._1.y}), (${ciphertext._2.x}, ${ciphertext._2.y})"
    )

    val decryptedMessage = curve.decrypt(privateKey, ciphertext)
    println(
      s"Decrypted Message: (${decryptedMessage.x}, ${decryptedMessage.y})"
    )

    // メッセージの署名と検証
    val signMessage = "Hello, ECC!".getBytes("UTF-8")
    val signature = curve.sign(privateKey, signMessage, basePoint)
    println(s"Signature: (r: ${signature._1}, s: ${signature._2})")

    val isValid = curve.verify(publicKey, signMessage, signature, basePoint)
    println(s"Signature valid: $isValid")
  }
}
