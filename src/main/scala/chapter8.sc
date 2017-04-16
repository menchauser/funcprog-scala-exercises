import proptest.Gen
import state.SimpleRNG

val c = Gen.choose(1, 10)

Gen.unit("a").sample.run(SimpleRNG(5))

Gen.listOfN(5, Gen.choose(1, 100)).sample.run(SimpleRNG(5))

val boolLists = c.listOfN(Gen.choose(1, 20))

boolLists.sample.run(SimpleRNG(1301))