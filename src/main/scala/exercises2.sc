import scala.annotation.tailrec

def fib(n: Int): Int = {
  @tailrec
  def go(n: Int, a: Int, b: Int): Int =
    if (n == 1) {
      a
    } else if (n == 2) {
      b
    } else {
      go(n - 1, b, a + b)
    }
  go(n, 0, 1)
}

for (n <- 1 to 10) println(fib(n))

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  def go(i: Int, as: Array[A]): Boolean =
      if (i == as.length) true
      else if (!ordered(as(i - 1), as(i))) false
      else go(i + 1, as)

  go(1, as)
}

isSorted(Array(1, 2, 2, 2, 2), (a: Int, b: Int) => a <= b)

def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
  b => f(a, b)

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => partial1(a, f)

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)

def add(x: Int, y: Int) = x + y

val ac = curry(add)
val inc = ac(1)
inc(5)

val uac = uncurry(ac)
uac(1, 2)

def compose[A, B, C](f: B => C, g: A => B): A => C =
  a => f(g(a))

val f = (x: Double) => math.Pi / 2 - x
val cos = f andThen math.sin
val myCos = compose(math.sin, f)
cos(30)
myCos(30)

List(1, 2)