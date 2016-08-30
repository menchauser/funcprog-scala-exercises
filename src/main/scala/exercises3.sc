import list._
import tree.{Branch, Leaf, Tree}

val xs = List(1, 2, 3, 4, 5)
println(xs)

List.drop(xs, 3)
List.setHead(99, xs)
List.drop(Nil, 3)
List.setHead(99, Nil)
List.tail(Nil)
List.tail(xs)
List.dropWhile(xs)(x => x < 4)
List.init(xs)

List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_,_))

List.length(List(1, 2, 3))

List.foldLeft(List(1, 2, 3), 0)(_ + _)

List.sum3(List(1, 2, 3, 4))
List.product3(List(1, 2, 3, 4))
List.length3(List(1, 2, 3, 4))
List.reverse(List(1, 2, 3, 4))

List.append(List(1, 2), List(3, 4))

List.concat(List(List(1, 3), List(2, 4)))

List.foldLeft(List(1, 2, 3), 0)(_ - _)
List.foldRight(List(1, 2, 3), 0)(_ - _)
List.foldRight2(List(1, 2, 3), 0)(_ - _)

scala.List(1, 2, 3).foldLeft(0)(_ - _)
scala.List(1, 2, 3).foldRight(0)(_ - _)

List.incList(List(1, 2, 3, 4))
List.doubleToString(List(1.0, 2.0, 3.0))

List.map(List(1, 2, 3))(_ * 2)

scala.List(1, 2, 3).map(_ * 2)

List.filter(List(1, 2, 3, 4, 5))(_ % 2 == 0)

List.flatMap(List(1, 2, 3))(i => List(i, i))

List.filterViaFlatmap(List(1, 2, 3, 4, 5))(_ % 2 == 0)

List.zipWithAdd(List(1, 2, 3), List(4, 5, 6))

List.zipWithAdd(List(1, 2, 3), List(4, 5))
List.zipWithAdd(List(1), List(4, 5, 6))

List.zipWith(List(1, 2, 3), List(4, 5))(_ * _)

List.startsWith(List(1, 2, 3, 4), List(1, 2))
List.startsWith(List(1, 2, 3, 4), List(2))

List.hasSubsequence(List(1, 2, 3, 4), List(1, 2))
List.hasSubsequence(List(1, 2, 3, 4), List(2, 3))
List.hasSubsequence(List(1, 2, 3, 4), List(4))
List.hasSubsequence(List(1, 2, 3, 4), List(2, 3, 4, 5))
List.hasSubsequence(List(1, 2, 3, 4), List(5))
List.hasSubsequence(Nil, List(5))


val testTree =
  Branch(
    Branch(
      Leaf(1),
      Leaf(2)),
    Leaf(3))

Tree.size(Leaf(1))
Tree.size(testTree)
Tree.maximum(testTree)
Tree.depth(testTree)
Tree.map(testTree)(_ * 2)

Tree.size2(Leaf(1))
Tree.size2(testTree)

Tree.maximum2(testTree)

Tree.depth2(testTree)

Tree.map2(testTree)(_ * 2)

