package arrow.laws.laws

import arrow.Kind
import arrow.core.Tuple3
import arrow.core.extensions.monoid
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.laws.generators.applicativeArbitrary
import arrow.laws.generators.functionAToBArbitrary
import arrow.typeclasses.Alternative
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid

object AlternativeLaws {

  fun <F> laws(AF: Alternative<F>, EQ: Eq<Kind<F, Int>>): List<Law> = laws(
    AF,
    applicativeArbitrary(Int.arbitrary(), AF),
    Int.arbitrary(),
    Int.monoid(),
    EQ
  )

  fun <F, A> laws(AF: Alternative<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>): List<Law> = laws(
    AF,
    arbF,
    functionAToBArbitrary(arbA),
    arbA,
    MA,
    EQ
  )

  fun <F, A> laws(
    AF: Alternative<F>,
    arbF: Arbitrary<Kind<F, A>>,
    arbAToA: Arbitrary<(A) -> A>,
    arbA: Arbitrary<A>,
    MA: Monoid<A>,
    EQ: Eq<Kind<F, A>>
  ): List<Law> =
    ApplicativeLaws.laws(AF, arbF, arbA, MA, arbAToA, EQ) + MonoidKLaws.laws(AF, arbF, EQ) + listOf(
      Law("Alternative Laws: Right Absorption", AF.alternativeRightAbsorption(arbAToA, EQ)),
      Law("Alternative Laws: Left Distributivity", AF.alternativeLeftDistributivity(arbF, arbAToA, EQ)),
      Law("Alternative Laws: Right Distributivity", AF.alternativeRightDistributivity(arbF, arbAToA, EQ))
    )

  fun <F, A> Alternative<F>.alternativeRightAbsorption(arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(
      applicativeArbitrary(arbAToA, this)
    ) { fa ->
      empty<A>().ap(fa).eqv(empty(), EQ)
    }

  fun <F, A> Alternative<F>.alternativeLeftDistributivity(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple3.arbitrary(
      arbF, arbF, arbAToA)
    ) { (fa, fa2, f) ->
      fa.combineK(fa2).map(f).eqv(fa.map(f).combineK(fa2.map(f)), EQ)
    }

  fun <F, A> Alternative<F>.alternativeRightDistributivity(
    arbF: Arbitrary<Kind<F, A>>,
    arbAToA: Arbitrary<(A) -> A>,
    EQ: Eq<Kind<F, A>>
  ): Property =
    forAll(Tuple3.arbitrary(
      arbF,
      applicativeArbitrary(arbAToA, this),
      applicativeArbitrary(arbAToA, this)
    )
    ) { (fa, ff, fg) ->
      fa.ap(ff.combineK(fg)).eqv(fa.ap(ff).combineK(fa.ap(fg)), EQ)
    }
}
