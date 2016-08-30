def failingFn(i: Int): Int = {
  val y: Int = throw new Exception("fail!")
  try {
    val x = 42 + 5
    x + y
  } catch {
    case e: Exception => 43
  }
}

//failingFn(12)

def failingFn2(i: Int): Int = {
  try {
    val x = 42 + 5
    x + ((throw new Exception("fail!")): Int)
  } catch {
    case e: Exception => 43
  }
}

failingFn2(12)

def mean(xs: Seq[Double]): Double =
  if (xs.isEmpty)
    throw new ArithmeticException("mean of empty list")
  else xs.sum / xs.length

mean(Seq(1.0, 30.0))
//mean(Seq())

import either._
def meanE(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty)
    Left("mean of empty list!")
  else Right(xs.sum / xs.length)


sealed case class Name(value: String)

sealed case class Age(value: Int)

case class Person(name: Name, age: Age)

def mkName(name: String): Either[String, Name] =
  if (name == null || name == "") Left("Name is empty")
  else Right(Name(name))

def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range")
  else Right(Age(age))

def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person)

mkPerson("John", 32)
mkPerson("", 19)
mkPerson("John", -1)
mkPerson("", -1)


def maybeTwice(b: Boolean, i: => Int) = {
  lazy val v = i
  if (b) v + v else 0
}

maybeTwice(true, {println("Hi"); 1})
