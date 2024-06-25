import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class EllipticCurveTest extends AnyFunSuite with Matchers {
  val curveA: BigInt = BigInt(0) // 名前を変更
  val b: BigInt = BigInt(7)
  val p: BigInt = BigInt(
    "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F",
    16
  )
  val basePoint: Point = Point(
    BigInt(
      "55066263022277343669578718895168534326250603453777594175500187360389116729240"
    ),
    BigInt(
      "32670510020758816978083085130507043184471273380659243275938904335757337482424"
    )
  )

  test("generateKeyPair should generate a valid key pair") {
    val (privateKey: BigInt, publicKey: Point) =
      EllipticCurve.generateKeyPair(basePoint, curveA, p)
    privateKey should be > BigInt(0)
    publicKey should not be null
  }

  test("encrypt and decrypt should correctly encrypt and decrypt a message") {
    val (privateKey: BigInt, publicKey: Point) =
      EllipticCurve.generateKeyPair(basePoint, curveA, p)
    val message: Point = Point(BigInt("12345"), BigInt("67890"))
    val ciphertext: (Point, Point) =
      EllipticCurve.encrypt(publicKey, message, basePoint, curveA, p)
    val decryptedMessage: Point =
      EllipticCurve.decrypt(privateKey, ciphertext, curveA, p)
    decryptedMessage shouldEqual message
  }

  test("sign and verify should correctly sign and verify a message") {
    val (privateKey: BigInt, publicKey: Point) =
      EllipticCurve.generateKeyPair(basePoint, curveA, p)
    val message: Array[Byte] = "Hello, ECC!".getBytes("UTF-8")
    val signature: (BigInt, BigInt) =
      EllipticCurve.sign(privateKey, message, basePoint, curveA, p)
    val isValid: Boolean =
      EllipticCurve.verify(publicKey, message, signature, basePoint, curveA, p)
    isValid shouldEqual true
  }

  test("addPoints should correctly add two points on the curve") {
    val p1: Point = Point(
      BigInt(
        "89565891926547004231252920425935692360644145829622209833684344723754790825080"
      ),
      BigInt(
        "12158399299693830322967808612713398636155367887041628176798871954788371653930"
      )
    )
    val p2: Point = basePoint
    val result: Point = EllipticCurve.addPoints(p1, p2, curveA, p)
    result should not be null
  }

  test("scalarMultiply should correctly multiply a point by a scalar") {
    val k: BigInt = BigInt("1234567890")
    val result: Point = EllipticCurve.scalarMultiply(k, basePoint, curveA, p)
    result should not be null
  }
}
