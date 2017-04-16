import state.{RNG, State}

package object proptest {

  case class Gen[A](sample: State[RNG, A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(n => Gen.listOfN(n, this))

  }

  object Gen {

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(State { rng =>
        val (i, rng2) = RNG.nonNegativeLessThan(stopExclusive - start)(rng)
        (start + i, rng2)
      })
    }

    def unit[A](a: => A): Gen[A] =
      Gen(State.unit(a))

    def boolean: Gen[Boolean] =
      Gen(State(RNG.boolean))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))


    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(b => if (b) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val threshold = g1._2 / (g1._2 + g2._2)

      Gen(State(RNG.double).flatMap(d => if (d < threshold) g1._1.sample else g2._1.sample))
    }
  }

  object Prop {
    type FailedCase = String
    type SuccessCount = Int
  }

  trait Prop {
    def check: Either[(Prop.FailedCase, Prop.SuccessCount), Prop.SuccessCount]

//    def &&(p: Prop): Prop = new Prop {
//      override def check: Boolean =
//        this.check && p.check
//    }
  }

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???


}
