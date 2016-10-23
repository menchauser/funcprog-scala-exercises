package object parallel {

  class Par[A] {

  }

  object Par {
    def unit[A](a: A): Par[A] = ???

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???

    def fork[A](a: => Par[A]): Par[A] = ???

    def run[A](a: Par[A]): A = ???
  }


  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption.getOrElse(0)
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      sum(l) + sum(r)
    }

  def sum2(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(sum2(l), sum2(r))(_ + _)
    }
  }

  def sum3(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(Par.fork(sum3(l)), Par.fork(sum3(r)))(_ + _)
    }
  }


}
