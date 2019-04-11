package arrow.laws.laws

import arrow.core.Tuple2
import arrow.core.extensions.eq
import arrow.propCheck.*
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.laws.generators.smallRangeArb
import arrow.typeclasses.Hash

object HashLaws {

  fun <F> laws(HF: Hash<F>, arbF: Arbitrary<F>): List<Law> =
    EqLaws.laws(HF, arbF) + listOf(
      Law("Hash Laws: Equality implies equal hash", HF.equalHash(arbF)),
      Law("Hash Laws: Multiple calls to hash should result in the same hash", HF.reproducableHash(arbF))
    )

  fun <F> Hash<F>.equalHash(arbF: Arbitrary<F>): Property =
    forAll(Tuple2.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF))) { (a, b) ->
      checkCoverage(
        cover(10.0, a.eqv(b), "a == b",
          !(a.eqv(b)) || a.hash() == b.hash()
        )
      )
    }

  fun <F> Hash<F>.reproducableHash(arbF: Arbitrary<F>): Property =
    forAll(arbF) { f ->
      f.hash().eqv(f.hash(), Int.eq())
    }
}