package arrow.laws.laws

import arrow.Kind2
import arrow.core.Tuple5
import arrow.core.andThen
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple5.arbitrary.arbitrary
import arrow.typeclasses.Bifunctor
import arrow.typeclasses.Eq

object BifunctorLaws {

  fun <F, A> laws(BF: Bifunctor<F>, arbF: Arbitrary<Kind2<F, A, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind2<F, A, A>>): List<Law> =
    listOf(
      Law("Bifunctor Laws: Identity", BF.identity(arbF, EQ)),
      Law("Bifunctor Laws: Composition", BF.composition(arbF, arbAToA, EQ))
    )

  fun <F, A> Bifunctor<F>.identity(arbF: Arbitrary<Kind2<F, A, A>>, EQ: Eq<Kind2<F, A, A>>): Property =
    forAll(arbF) { fa ->
      fa.bimap({ it }, { it }).eqv(fa, EQ)
    }

  fun <F, A> Bifunctor<F>.composition(arbF: Arbitrary<Kind2<F, A, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind2<F, A, A>>): Property =
    forAll(
      Tuple5.arbitrary(
        arbF,
        arbAToA,
        arbAToA,
        arbAToA,
        arbAToA
      )
    ) { (fa, ff, g, x, y) ->
      fa.bimap(ff, g).bimap(x, y).eqv(fa.bimap(ff andThen x, g andThen y), EQ)
    }

}
