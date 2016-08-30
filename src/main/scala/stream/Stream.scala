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
}