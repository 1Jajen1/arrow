package arrow.test.laws

import arrow.Kind
import arrow.core.Tuple2
import arrow.core.toT
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.typeclasses.Divide
import arrow.typeclasses.Eq

object DivideLaws {

  fun <F, A> laws(DF: Divide<F>, arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(DF, arbF, arbAToA, arbAToA, EQ)

  fun <F, A, B> laws(
    DF: Divide<F>,
    arbF: Arbitrary<Kind<F, A>>,
    arbAToB: Arbitrary<(A) -> B>,
    arbBToA: Arbitrary<(B) -> A>,
    EQ: Eq<Kind<F, A>>
  ): List<Law> = ContravariantLaws.laws(DF, arbF, arbAToB, arbBToA, EQ) + listOf(
    Law("Divide laws: Associative", DF.associative(arbF, EQ))
  )

  fun <A> delta(a: A): Tuple2<A, A> = a toT a

  fun <F, A> Divide<F>.associative(
    arbF: Arbitrary<Kind<F, A>>,
    EQ: Eq<Kind<F, A>>
  ): Property =
    forAll(arbF) { fa ->
      val a = divide<A, A, A>(
        fa,
        divide(fa, fa) { delta(it) }
      ) { delta(it) }
      val b = divide<A, A, A>(
        divide(fa, fa) { delta(it) },
        fa
      ) { delta(it) }

      a.eqv(b, EQ)
    }
}
