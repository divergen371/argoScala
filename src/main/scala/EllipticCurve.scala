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
}
