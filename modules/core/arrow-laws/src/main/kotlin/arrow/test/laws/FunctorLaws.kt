package arrow.test.laws

import arrow.Kind
import arrow.core.Tuple3
import arrow.core.andThen
import arrow.core.identity
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Functor

object FunctorLaws {

  fun <F, A> laws(FF: Functor<F>, arbF: Arbitrary<Kind<F, A>>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(FF, arbF, arbAtoA, arbAtoA, EQ)

  fun <F, A, B> laws(FF: Functor<F>, arbF: Arbitrary<Kind<F, A>>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>): List<Law> =
    InvariantLaws.laws(FF, arbF, arbAtoB, arbBtoA, EQ) + listOf(
      Law("Functor Laws: Covariant Identity", FF.covariantIdentity(arbF, EQ)),
      Law("Functor Laws: Covariant Composition", FF.covariantComposition(arbF, arbAtoB, arbBtoA, EQ))
    )

  fun <F, A> Functor<F>.covariantIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      fa.map(::identity).eqv(fa, EQ)
    }

  fun <F, A, B> Functor<F>.covariantComposition(arbF: Arbitrary<Kind<F, A>>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(
      Tuple3.arbitrary(
        arbF,
        arbAtoB,
        arbBtoA
      )
    ) { (fa, f, g) ->
      fa.map(f).map(g).eqv(fa.map(f andThen g), EQ)
    }
}
