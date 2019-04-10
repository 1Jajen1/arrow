package arrow.test.laws

import arrow.Kind
import arrow.core.Tuple2
import arrow.core.extensions.monoid
import arrow.mtl.typeclasses.MonadState
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.test.generators.applicativeArbitrary
import arrow.test.generators.functionAToBArbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid

object MonadStateLaws {

  fun <F> laws(MS: MonadState<F, Int>, EQ_UNIT: Eq<Kind<F, Unit>>, EQ: Eq<Kind<F, Int>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> = laws(
    MS,
    Int.arbitrary(),
    EQ, EQ_UNIT, EQ, unsafeRun
  )

  fun <F, S> laws(MS: MonadState<F, S>, arbS: Arbitrary<S>, EQ_S: Eq<Kind<F, S>>, EQ_UNIT: Eq<Kind<F, Unit>>, EQ: Eq<Kind<F, Int>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> = laws(
    MS,
    Int.arbitrary(),
    arbS,
    Int.monoid(),
    functionAToBArbitrary(Int.arbitrary()),
    EQ_S, EQ_UNIT, EQ, unsafeRun
  )

  fun <F, S, A> laws(MS: MonadState<F, S>, arbA: Arbitrary<A>, arbS: Arbitrary<S>, MA: Monoid<A>, arbAToA: Arbitrary<(A) -> A>, EQ_S: Eq<Kind<F, S>>, EQ_UNIT: Eq<Kind<F, Unit>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MS, applicativeArbitrary(arbA, MS), arbA, arbS, MA, arbAToA, arbAToA, EQ_S, EQ_UNIT, EQ, unsafeRun)

  fun <F, S, A> laws(MS: MonadState<F, S>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbS: Arbitrary<S>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_S: Eq<Kind<F, S>>, EQ_UNIT: Eq<Kind<F, Unit>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MS, arbF, arbA, arbS, MA, arbAtoA, arbAtoA, EQ_S, EQ_UNIT, EQ, unsafeRun)

  fun <F, S, A, B> laws(MS: MonadState<F, S>, arbA: Arbitrary<A>, arbS: Arbitrary<S>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ_S: Eq<Kind<F, S>>, EQ_UNIT: Eq<Kind<F, Unit>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MS, applicativeArbitrary(arbA, MS), arbA, arbS, MA, arbAToB, arbBToA, EQ_S, EQ_UNIT, EQ, unsafeRun)

  fun <F, S, A, B> laws(MS: MonadState<F, S>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbS: Arbitrary<S>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ_S: Eq<Kind<F, S>>, EQ_UNIT: Eq<Kind<F, Unit>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> =
    MonadLaws.laws(MS, arbF, arbA, MA, arbAToB, arbBToA, EQ, unsafeRun) + listOf(
      Law("Monad State Laws: idempotence", MS.monadStateGetIdempotent(EQ_S)),
      Law("Monad State Laws: set twice eq to set once the last element", MS.monadStateSetTwice(arbS, EQ_UNIT)),
      Law("Monad State Laws: set get", MS.monadStateSetGet(arbS, EQ_S)),
      Law("Monad State Laws: get set", MS.monadStateGetSet(EQ_UNIT))
    )

  fun <F, S> MonadState<F, S>.monadStateGetIdempotent(EQ: Eq<Kind<F, S>>): Property =
    get().flatMap { get() }.eqv(get(), EQ)

  fun <F, S> MonadState<F, S>.monadStateSetTwice(arbS: Arbitrary<S>, EQ: Eq<Kind<F, Unit>>): Property =
    forAll(Tuple2.arbitrary(arbS, arbS)) { (s, t) ->
      set(s).flatMap { set(t) }.eqv(set(t), EQ)
    }

  fun <F, S> MonadState<F, S>.monadStateSetGet(arbS: Arbitrary<S>, EQ: Eq<Kind<F, S>>): Property =
    forAll(arbS) { s ->
      set(s).flatMap { get() }.eqv(set(s).flatMap { just(s) }, EQ)
    }

  fun <F, S> MonadState<F, S>.monadStateGetSet(EQ: Eq<Kind<F, Unit>>): Property =
    get().flatMap { set(it) }.eqv(just(Unit), EQ)

}
