object Main {
  def main(args: Array[String]): Unit = {

    checkPrime()
  }

  def checkPrime(): Unit = {
    val startTime = System.nanoTime()

    (1L to 1000L).foreach { numberToCheck =>
      if (AKSPrimalityTest.aksPrimalityTest(numberToCheck)) {
        println(s"$numberToCheck は素数です。")
      } else {
        println(s"$numberToCheck は素数ではありません。")
      }
    }

    val endTime = System.nanoTime()

    val elapsedTimeMillis = (endTime - startTime) / 1000000.0
    println(s"処理時間: $elapsedTimeMillis ミリ秒")
  }
}
