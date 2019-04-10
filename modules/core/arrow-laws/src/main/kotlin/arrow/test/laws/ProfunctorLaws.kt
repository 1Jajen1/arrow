package arrow.test.laws

import arrow.Kind2
import arrow.core.Tuple3
import arrow.core.Tuple5
import arrow.core.andThen
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.propCheck.instances.tuple5.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Profunctor

object ProfunctorLaws {

  fun <F, A> laws(PF: Profunctor<F>, arbF: Arbitrary<Kind2<F, A, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind2<F, A, A>>): List<Law> =
    listOf(
      Law("Profunctor Laws: Identity", PF.identity(arbF, EQ)),
      Law("Profunctor Laws: Composition", PF.composition(arbF, arbAToA, EQ)),
      Law("Profunctor Laws: Lmap Identity", PF.lMapIdentity(arbF, EQ)),
      Law("Profunctor Laws: Rmap Identity", PF.rMapIdentity(arbF, EQ)),
      Law("Profunctor Laws: Lmap Composition", PF.lMapComposition(arbF, arbAToA, EQ)),
      Law("Profunctor Laws: Rmap Composition", PF.rMapComposition(arbF, arbAToA, EQ))
    )

  fun <F, A> Profunctor<F>.identity(arbF: Arbitrary<Kind2<F, A, A>>, EQ: Eq<Kind2<F, A, A>>): Property =
    forAll(arbF) { fa ->
      fa.dimap<A, A, A, A>({ it }, { it }).eqv(fa, EQ)
    }

  fun <F, A> Profunctor<F>.composition(arbF: Arbitrary<Kind2<F, A, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind2<F, A, A>>): Property =
    forAll(
      Tuple5.arbitrary(
        arbF,
        arbAToA,
        arbAToA,
        arbAToA,
        arbAToA
      )
    ) { (fa, ff, g, x, y) ->
      fa.dimap(ff, g).dimap(x, y).eqv(fa.dimap(x andThen ff, g andThen y), EQ)
    }

  fun <F, A> Profunctor<F>.lMapIdentity(arbF: Arbitrary<Kind2<F, A, A>>, EQ: Eq<Kind2<F, A, A>>): Property =
    forAll(arbF) { fa ->
      fa.lmap<A, A, A> { it }.eqv(fa, EQ)
    }

  fun <F, A> Profunctor<F>.rMapIdentity(arbF: Arbitrary<Kind2<F, A, A>>, EQ: Eq<Kind2<F, A, A>>): Property =
    forAll(arbF) { fa ->
      fa.rmap { it }.eqv(fa, EQ)
    }

  fun <F, A> Profunctor<F>.lMapComposition(arbF: Arbitrary<Kind2<F, A, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind2<F, A, A>>): Property =
    forAll(
      Tuple3.arbitrary(
        arbF,
        arbAToA,
        arbAToA
      )
    ) { (fa, ff, g) ->
      fa.lmap(g).lmap(ff).eqv(fa.lmap(ff andThen g), EQ)
    }

  fun <F, A> Profunctor<F>.rMapComposition(arbF: Arbitrary<Kind2<F, A, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind2<F, A, A>>): Property =
    forAll(
      Tuple3.arbitrary(
        arbF,
        arbAToA,
        arbAToA
      )
    ) { (fa, ff, g) ->
      fa.rmap(ff).rmap(g).eqv(fa.rmap(ff andThen g), EQ)
    }
}
