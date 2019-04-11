package arrow.laws.laws

import arrow.Kind
import arrow.core.Tuple3
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.SemigroupK

object SemigroupKLaws {

  fun <F, A> laws(SGK: SemigroupK<F>, arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): List<Law> =
    listOf(Law("SemigroupK: associativity", SGK.semigroupKAssociative(arbF, EQ)))

  fun <F, A> SemigroupK<F>.semigroupKAssociative(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple3.arbitrary(arbF, arbF, arbF)) { (a, b, c) ->
      a.combineK(b).combineK(c).eqv(a.combineK(b.combineK(c)), EQ)
    }
}
