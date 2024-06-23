import scala.collection.immutable.ArraySeq
val arraySeq: ArraySeq[Int] = ArraySeq.from(0 to 10)
arraySeq.foreach { i =>
  println(s"i $i")
}
for (a <- arraySeq) {
  println(a)
}

