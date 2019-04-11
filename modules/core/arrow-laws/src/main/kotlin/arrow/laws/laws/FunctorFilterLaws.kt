package arrow.laws.laws

import arrow.Kind
import arrow.core.Option
import arrow.core.Some
import arrow.core.Tuple2
import arrow.core.Tuple3
import arrow.laws.generators.functionAToBArbitrary
import arrow.laws.generators.makeFunAtoA
import arrow.mtl.typeclasses.FunctorFilter
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.option.arbitrary.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Eq

object FunctorFilterLaws {

  fun <F, A> laws(FF: FunctorFilter<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): List<Law> =
    laws(FF, arbF, arbA, functionAToBArbitrary(arbA), EQ)

  fun <F, A> laws(FF: FunctorFilter<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(FF, arbF, arbA, arbAtoA, arbAtoA, EQ)

  fun <F, A, B> laws(FFF: FunctorFilter<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>): List<Law> =
    FunctorLaws.laws(FFF, arbF, arbAToB, arbBToA, EQ) + listOf(
      Law("Functor Filter: mapFilter composition", FFF.mapFilterComposition(arbF, arbA, EQ)),
      Law("Functor Filter: mapFilter map consistency", FFF.mapFilterMapConsistency(arbF, makeFunAtoA(arbAToB, arbBToA), EQ))
    )

  fun <F, A> FunctorFilter<F>.mapFilterComposition(arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(
      Tuple3.arbitrary(
        arbF,
        functionAToBArbitrary<A, Option<A>>(Option.arbitrary(arbA)),
        functionAToBArbitrary<A, Option<A>>(Option.arbitrary(arbA))
      )
    ) { (fa, f, g) ->
      fa.mapFilter(f).mapFilter(g).eqv(fa.mapFilter { a -> f(a).flatMap(g) }, EQ)
    }

  fun <F, A> FunctorFilter<F>.mapFilterMapConsistency(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(
      Tuple2.arbitrary(
        arbF,
        arbAToA
      )
    ) { (fa, f) ->
      fa.mapFilter { Some(f(it)) }.eqv(fa.map(f), EQ)
    }
}
