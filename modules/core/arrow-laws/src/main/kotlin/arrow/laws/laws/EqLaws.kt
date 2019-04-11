package arrow.laws.laws

import arrow.core.Tuple2
import arrow.core.Tuple3
import arrow.propCheck.*
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.laws.generators.smallRangeArb
import arrow.typeclasses.Eq

object EqLaws {

  fun <F> laws(EQ: Eq<F>, arbF: Arbitrary<F>): List<Law> =
    listOf(
      Law("Eq Laws: reflexivity", EQ.reflexivityEquality(arbF)),
      Law("Eq Laws: commutativity", EQ.commutativeEquality(arbF)),
      Law("Eq Laws: transitivity", EQ.transitiveEquality(arbF))
    )

  fun <F> Eq<F>.reflexivityEquality(arbF: Arbitrary<F>): Property =
    forAll(arbF) { f ->
      f.eqv(f, this)
    }

  fun <F> Eq<F>.commutativeEquality(arbF: Arbitrary<F>): Property =
    forAll(Tuple2.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF))) { (a, b) ->
      checkCoverage(
        cover(10.0, a.eqv(b), "a == b",
          a.eqv(b) == b.eqv(a)
        )
      )
    }

  fun <F> Eq<F>.transitiveEquality(arbF: Arbitrary<F>): Property =
    forAll(Tuple3.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF), smallRangeArb(arbF))) { (a, b, c) ->
      checkCoverage(
        cover(1.0, a.eqv(b) && b.eqv(c), "a == b && b == c",
          !(a.eqv(b) && b.eqv(c)) || a.eqv(c)
        )
      )
    }
}