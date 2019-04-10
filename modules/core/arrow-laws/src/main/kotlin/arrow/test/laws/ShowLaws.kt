package arrow.test.laws

import arrow.core.Tuple2
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.checkCoverage
import arrow.propCheck.cover
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.test.generators.smallRangeArb
import arrow.typeclasses.Eq
import arrow.typeclasses.Show

object ShowLaws {

  fun <F> laws(S: Show<F>, EQ: Eq<F>, arbF: Arbitrary<F>): List<Law> =
    listOf(
      Law("Show Laws: equality", S.equalShow(EQ, arbF))
    )

  fun <F> Show<F>.equalShow(EQ: Eq<F>, arbF: Arbitrary<F>): Property =
    forAll(Tuple2.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF))) { (a, b) ->
      checkCoverage(
        cover(10.0, EQ.run { a.eqv(b) }, "Equal pairs",
          !(a.show() == b.show()) || EQ.run { a.eqv(b) }
        )
      )
    }
}