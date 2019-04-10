package arrow.test.laws

import arrow.Kind
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.typeclasses.Divisible
import arrow.typeclasses.Eq

object DivisibleLaws {
  fun <F, A> laws(DF: Divisible<F>, arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(DF, arbF, arbAToA, arbAToA, EQ)

  fun <F, A, B> laws(
    DF: Divisible<F>,
    arbF: Arbitrary<Kind<F, A>>,
    arbAToB: Arbitrary<(A) -> B>,
    arbBToA: Arbitrary<(B) -> A>,
    EQ: Eq<Kind<F, A>>
  ): List<Law> = DivideLaws.laws(DF, arbF, arbAToB, arbBToA, EQ) + listOf(
    Law("Divisible laws: Left identity", DF.leftIdentity(arbF, EQ)),
    Law("Divisible laws: Right identity", DF.rightIdentity(arbF, EQ))
  )

  fun <F, A> Divisible<F>.leftIdentity(
    arbF: Arbitrary<Kind<F, A>>,
    EQ: Eq<Kind<F, A>>
  ): Property =
    forAll(arbF) { fa ->
      divide<A, A, A>(fa, conquer()) { DivideLaws.delta(it) }.eqv(fa, EQ)
    }

  fun <F, A> Divisible<F>.rightIdentity(
    arbF: Arbitrary<Kind<F, A>>,
    EQ: Eq<Kind<F, A>>
  ): Property =
    forAll(arbF) { fa ->
      divide<A, A, A>(conquer(), fa) { DivideLaws.delta(it) }.eqv(fa, EQ)
    }
}
