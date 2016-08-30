package tree

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(onLeaf: A => B)(onBranch: (B, B) => B): B = tree match {
    case Leaf(a) => onLeaf(a)
    case Branch(left, right) => onBranch(fold(left)(onLeaf)(onBranch), fold(right)(onLeaf)(onBranch))
  }

  def size2[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((l, r) => 1 + l + r)

  def maximum2(tree: Tree[Int]): Int =
    fold(tree)(identity)(_ max _)

  def depth2(tree: Tree[Int]): Int =
    fold(tree)(_ => 1)(1 + _.max(_))

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))

}