package arrow.test.laws

import arrow.Kind
import arrow.core.Tuple2
import arrow.core.Tuple6
import arrow.core.extensions.monoid
import arrow.data.extensions.list.foldable.fold
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple6.arbitrary.arbitrary
import arrow.test.generators.applicativeArbitrary
import arrow.test.generators.functionAToBArbitrary
import arrow.typeclasses.Applicative
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid

object ApplicativeLaws {

  fun <F> laws(AP: Applicative<F>, EQ: Eq<Kind<F, Int>>): List<Law> = laws(
    AP,
    applicativeArbitrary(Int.arbitrary(), AP),
    Int.arbitrary(),
    Int.monoid(),
    functionAToBArbitrary(Int.arbitrary()),
    EQ
  )

  fun <F, A> laws(AP: Applicative<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(AP, applicativeArbitrary(arbA, AP), arbA, MA, arbAtoA, arbAtoA, EQ)

  fun <F, A> laws(AP: Applicative<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(AP, arbF, arbA, MA, arbAtoA, arbAtoA, EQ)

  fun <F, A, B> laws(AP: Applicative<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(AP, applicativeArbitrary(arbA, AP), arbA, MA, arbAtoB, arbBtoA, EQ)

  fun <F, A, B> laws(AP: Applicative<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>): List<Law> =
    FunctorLaws.laws(AP, arbF, arbAtoB, arbBtoA, EQ) + listOf(
      Law("Applicative Laws: ap identity", AP.apIdentity(arbF, EQ)),
      Law("Applicative Laws: homomorphism", AP.homomorphism(arbA, EQ)),
      Law("Applicative Laws: interchange", AP.interchange(arbA, EQ)),
      Law("Applicative Laws: map derived", AP.mapDerived(arbF, arbA, EQ)),
      Law("Applicative Laws: cartesian builder map", AP.cartesianBuilderMap(arbA, MA, EQ)),
      Law("Applicative Laws: cartesian builder tupled", AP.cartesianBuilderTupled(arbA, MA, EQ))
    )

  fun <F, A> Applicative<F>.apIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      fa.ap(just({ a: A -> a })).eqv(fa, EQ)
    }

  fun <F, A> Applicative<F>.homomorphism(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<A, A>(arbA),
      arbA
    )) { (ab, a) ->
      just(a).ap(just(ab)).eqv(just(ab(a)), EQ)
    }

  fun <F, A> Applicative<F>.interchange(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      applicativeArbitrary(functionAToBArbitrary<A, A>(arbA), this),
      arbA
    )) { (fa, a) ->
      just(a).ap(fa).eqv(fa.ap(just({ x: (A) -> A -> x(a) })), EQ)
    }

  fun <F, A> Applicative<F>.mapDerived(arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      arbF,
      functionAToBArbitrary<A, A>(arbA))
    ) { (fa, f) ->
      fa.map(f).eqv(fa.ap(just(f)), EQ)
    }

  fun <F, A> Applicative<F>.cartesianBuilderMap(arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple6.arbitrary(
      arbA, arbA, arbA, arbA, arbA, arbA
    )) { (a, b, c, d, e, f) ->
      map(just(a), just(b), just(c), just(d), just(e), just(f)) { listOf(it.a, it.b, it.c, it.d, it.e, it.f).fold(MA) }
        .eqv(just(listOf(a, b, c, d, e, f).fold(MA)), EQ)
    }

  fun <F, A> Applicative<F>.cartesianBuilderTupled(arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple6.arbitrary(
      arbA, arbA, arbA, arbA, arbA, arbA
    )) { (a, b, c, d, e, f) ->
      tupled(just(a), just(b), just(c), just(d), just(e), just(f)).map { listOf(it.a, it.b, it.c, it.d, it.e, it.f).fold(MA) }
        .eqv(just(listOf(a, b, c, d, e, f).fold(MA)), EQ)
    }
}
