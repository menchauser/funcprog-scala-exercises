package list

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, tl) => tl
  }

  def setHead[A](h: A, xs: List[A]): List[A] = Cons(h, tail(xs))

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, y) => Cons(x, init(y))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, n) => n + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def length3[A](as: List[A]): Int = foldLeft(as, 0)((n, _) => n + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((xs, x) => Cons(x, xs))

  // foldLeft in terms of foldRight
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = ???

  // foldRight in terms of foldLeft
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  def append[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs, ys)(Cons(_, _))

  def concat[A](xxs: List[List[A]]): List[A] = foldRight(xxs, Nil: List[A])(append)

  def incList(is: List[Int]): List[Int] = foldRight(is, Nil: List[Int])((n, rs) => Cons(n + 1, rs))

  def doubleToString(ds: List[Double]): List[String] = foldRight2(ds, Nil: List[String])((d, ss) => Cons(d.toString, ss))

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight2(as, Nil: List[B])((a, bs) => Cons(f(a), bs))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight2(as, Nil: List[A]) { (a, as) =>
    if (f(a)) Cons(a, as)
    else as
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterViaFlatmap[A](as: List[A])(f: A => Boolean): List[A] =
    List.flatMap(as) { a =>
      if (f(a)) Cons(a, Nil)
      else Nil
    }

  def zipWithAdd(as: List[Int], bs: List[Int]): List[Int] = zipWith(as, bs)(_ + _)

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Cons(a, aas), Cons(b, bbs)) => Cons(f(a, b), zipWith(aas, bbs)(f))
      case _ => Nil
    }

  def startsWith[A](xs: List[A], prefix: List[A]): Boolean = {
    require(prefix != Nil, "cannot check Nil prefix")
    length(filter(zipWith(xs, prefix)(_ == _))(identity)) ==
      length(prefix)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil if sub == Nil => true
      case Nil => false
      case Cons(_, _) if startsWith(sup, sub) => true
      case Cons(_, tl) => hasSubsequence(tl, sub)
    }

}

