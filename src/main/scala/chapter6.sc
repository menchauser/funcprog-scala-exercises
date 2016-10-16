import state.{RNG, SimpleRNG}

import scala.util.Random

SimpleRNG(1).nextInt
SimpleRNG(1).nextInt
SimpleRNG(1).nextInt._2.nextInt
SimpleRNG(1).nextInt._2.nextInt
SimpleRNG(1).nextInt._2.nextInt._2.nextInt

RNG.double3(SimpleRNG(15))

RNG.ints(10)(SimpleRNG(15))

RNG.doubleViaMap(SimpleRNG(15))
RNG.doubleViaMap(SimpleRNG(15))
RNG.doubleViaMap(SimpleRNG(16))

RNG.both(RNG.int, RNG.doubleViaMap)(SimpleRNG(15))

RNG.nonNegativeLessThan(4)(SimpleRNG(15))


def rollDie: RNG.Rand[Int] =
  RNG.map_(RNG.nonNegativeLessThan(6))(_ + 1)

val zero = rollDie(SimpleRNG(15))
