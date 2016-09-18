package stream

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A]

  def take(n: Int): Stream[A]

  def drop(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A]

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)(p(_) || _)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]) { (a, b) =>
      if (p(a)) Cons(() => a, () => b)
      else Empty
    }

  def headOption2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]) { (a, bs) =>
      if (p(a)) Cons(() => a, () => bs)
      else bs
    }

  def append[B >: A](that: () => Stream[B]): Stream[B] = {
    foldRight(that)((a, b) => () => Cons(() => a, b)).apply()
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    map(f).foldRight(Empty: Stream[B])((as, b) => as.append(() => b))
  }


  def map2[B](f: A => B): Stream[B] = {
    Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def take2(n: Int): Stream[A] = ???

  def takeWhile3(p: (A) => Boolean): Stream[A] = ???

  def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] = ???

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???
}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = Nil

  override def take(n: Int): Stream[Nothing] = this

  override def drop(n: Int): Stream[Nothing] = this

  override def takeWhile(p: (Nothing) => Boolean): Stream[Nothing] = this
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toList: List[A] = h() :: t().toList

  override def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else Cons(h, () => t().take(n - 1))

  override def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else t().drop(n - 1)

  override def takeWhile(p: (A) => Boolean): Stream[A] =
    if (p(h())) Cons(h, () => t().takeWhile(p))
    else Empty

}

object Stream {
  def cons[A](hd: A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(b, go(b, a + b))

    cons(0, go(0, 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) map { case (a, s) =>
      cons(a, unfold(s)(f))
    } getOrElse Empty
  }

  def fibs2: Stream[Int] = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def constant2[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  def ones2: Stream[Int] = unfold(1)(x => Some(x, x))
}