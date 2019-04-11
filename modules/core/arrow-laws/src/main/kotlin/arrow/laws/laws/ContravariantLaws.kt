package arrow.laws.laws

import arrow.Kind
import arrow.core.Tuple3
import arrow.core.compose
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.laws.generators.makeFunAtoA
import arrow.typeclasses.Contravariant
import arrow.typeclasses.Eq

object ContravariantLaws {

  fun <F, A> laws(CF: Contravariant<F>, arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(CF, arbF, arbAToA, arbAToA, EQ)

  fun <F, A, B> laws(CF: Contravariant<F>, arbF: Arbitrary<Kind<F, A>>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>): List<Law> =
    InvariantLaws.laws(CF, arbF, arbAToB, arbBToA, EQ) + listOf(
      Law("Contravariant Laws: Contravariant Identity", CF.identity(arbF, EQ)),
      Law("Contravariant Laws: Contravariant Composition", CF.composition(arbF, makeFunAtoA(arbAToB, arbBToA), EQ))
    )

  fun <F, A> Contravariant<F>.identity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      @Suppress("ExplicitItLambdaParameter")
      fa.contramap { it: A -> it }.eqv(fa, EQ)
    }

  fun <F, A> Contravariant<F>.composition(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(
      Tuple3.arbitrary(
        arbF,
        arbAToA,
        arbAToA
      )
    ) { (fa, f, g) ->
      fa.contramap(f).contramap(g).eqv(fa.contramap(f compose g), EQ)
    }

}
