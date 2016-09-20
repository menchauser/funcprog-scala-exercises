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

