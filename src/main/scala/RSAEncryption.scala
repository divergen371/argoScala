import java.security.{KeyPairGenerator, PrivateKey, PublicKey}
import java.util.Base64
import javax.crypto.Cipher
import scala.util.Try

object RSAEncryption {
  def main(args: Array[String]): Unit = {
    val result = for {
      keys <- generateKeyPair
      message = "Hello, Scala RSA!"
      encrypted <- encrypt(message.getBytes, keys.publicKey)
      encryptedBase64 = encodeBase64(encrypted)
      decryptedBytes <- decodeBase64(encryptedBase64).flatMap(
        decrypt(_, keys.privateKey)
      )
    } yield {
      (encryptedBase64, new String(decryptedBytes))
    }
    result match {
      case Right((encryptedMessage, decryptedMessage)) =>
        println(s"Encrypted Message (Base64): $encryptedMessage")
        println(s"Decrypted Message: $decryptedMessage")
      case Left(error) =>
        println(s"An error occurred: ${error.getMessage}")
    }
  }

  /** RSA鍵ペア（公開鍵と秘密鍵）を生成します。
    *
    * @return
    *   鍵ペア生成のエラーが発生した場合はThrowable、それ以外の場合はRSAKeys（公開鍵と秘密鍵を含む）を返します。
    */
  def generateKeyPair: Either[Throwable, RSAKeys] = {
    Try {
      val keyGen = KeyPairGenerator.getInstance("RSA")
      keyGen.initialize(2048)
      val pair = keyGen.generateKeyPair()
      RSAKeys(pair.getPublic, pair.getPrivate)
    }.toEither
  }

  /** 提供された公開鍵を使用してデータを暗号化します。
    *
    * @param data
    *   暗号化するデータのバイト配列。
    *
    * @param publicKey
    *   暗号化に使用する公開鍵。
    *
    * @return
    *   暗号化のエラーが発生した場合はThrowable、それ以外の場合は暗号化されたデータのバイト配列を返します。
    */
  def encrypt(
      data: Array[Byte],
      publicKey: PublicKey
  ): Either[Throwable, Array[Byte]] = {
    Try {
      val cipher = Cipher.getInstance("RSA")
      cipher.init(Cipher.ENCRYPT_MODE, publicKey)
      cipher.doFinal(data)
    }.toEither
  }

  /** 提供された秘密鍵を使用してデータを復号します。
    *
    * @param data
    *   暗号化されたデータのバイト配列。
    *
    * @param privateKey
    *   復号に使用する秘密鍵。
    *
    * @return
    *   復号のエラーが発生した場合はThrowable、それ以外の場合は復号されたデータのバイト配列を返します。
    */
  def decrypt(
      data: Array[Byte],
      privateKey: PrivateKey
  ): Either[Throwable, Array[Byte]] = {
    Try {
      val cipher = Cipher.getInstance("RSA")
      cipher.init(Cipher.DECRYPT_MODE, privateKey)
      cipher.doFinal(data)
    }.toEither
  }

  /** データをBase64文字列にエンコードします。
    *
    * @param data
    *   エンコードするデータのバイト配列。
    *
    * @return
    *   エンコードされたBase64文字列を返します。
    */
  def encodeBase64(data: Array[Byte]): String = Base64.getEncoder
    .encodeToString(data)

  /** Base64文字列をバイト配列にデコードします。
    *
    * @param data
    *   デコードするBase64文字列。
    *
    * @return
    *   デコードのエラーが発生した場合はThrowable、それ以外の場合はデコードされたデータのバイト配列を返します。
    */
  def decodeBase64(data: String): Either[Throwable, Array[Byte]] = {
    Try {
      Base64.getDecoder.decode(data)
    }.toEither
  }

  case class RSAKeys(publicKey: PublicKey, privateKey: PrivateKey)
}
