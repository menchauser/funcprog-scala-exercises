import option._

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs)
    .flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

mean(Seq(1, 2, 3))
variance(Seq(1, 2, 3))
variance(Seq())

def absO: Option[Double] => Option[Double] = Option.lift(math.abs)
absO(Some(-5.0))


val a = Some(1)
val b = Some(99)
val c = Some(3)

val c1 = c.map(c => c :: Nil)
val b1 = b.flatMap(b => c1.map(c1 => b :: c1))
val a1 = a.flatMap(a => b1.map(b1 => a :: b1))

for {
  nil <- Some(Nil: List[Int])
  c1 <- c
  b1 <- b
  a1 <- a
} yield a1 :: b1 :: c1 :: nil

a.map(
  a => List(a, b.map(
    b => List(b, c.map(
      c => List(c, Nil: List[Int]))))))

val xs = List(a, b, c)

xs.foldRight(Some(Nil): Option[List[Int]]) { (x, l) =>
  l.flatMap(l => x.map(_ :: l))
}

Option.sequence(List(Some(1), Some(2), Some(15), Some(4)))