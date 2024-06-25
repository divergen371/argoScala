/** ECCTestオブジェクトは、楕円曲線暗号の基本操作を実演する。
  */
object ECCTest {

  /** プログラムのエントリポイント。楕円曲線上の点の加算を実演する。
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

    // 楕円曲線上の点を定義
    val point1 = Point(
      BigInt(
        "55066263022277343669578718895168534326250603453777594175500187360389116729240"
      ),
      BigInt(
        "32670510020758816978083085130507043184471273380659243275938904335757337482424"
      )
    )
    val point2 = Point(
      BigInt(
        "89565891926547004231252920425935692360644145829622209833684344723754790825080"
      ),
      BigInt(
        "12158399299693830322967808612713398636155367887041628176798871954788371653930"
      )
    )

    // 点の加算
    val resultPoint = curve.addPoints(point1, point2)

    // 結果を表示
    println(s"Resulting Point: (${resultPoint.x}, ${resultPoint.y})")
  }
}
