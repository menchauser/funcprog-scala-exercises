import stream._

val s = Stream(1, 2, 3, 4, 5)
s.toList

s.take(3).toList

s.drop(3).toList

s.takeWhile(_ < 3).toList

s.forAll(_ > 0)

s.forAll(_ < 4)

s.takeWhile2(_ < 3).toList

s.headOption2

s.map(_ + 1).toList

s.filter(_ % 2 == 0).toList

val s2 = () => {
  println("Ooops"); Stream(4, 3, 2, 1)
}
val res = s.append(s2)
res.toList

val fmRes = s.flatMap(a => Stream(a, a + 1))
fmRes.toList
