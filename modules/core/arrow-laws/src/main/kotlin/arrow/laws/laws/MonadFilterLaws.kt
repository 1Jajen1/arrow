package arrow.laws.laws

import arrow.Kind
import arrow.core.Tuple2
import arrow.core.extensions.monoid
import arrow.laws.generators.applicativeArbitrary
import arrow.laws.generators.functionAToBArbitrary
import arrow.mtl.typeclasses.MonadFilter
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid

object MonadFilterLaws {

  fun <F> laws(MF: MonadFilter<F>, EQ: Eq<Kind<F, Int>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> = laws(
    MF,
    Int.arbitrary(),
    Int.monoid(),
    functionAToBArbitrary(Int.arbitrary()),
    EQ, unsafeRun
  )

  fun <F, A> laws(MF: MonadFilter<F>, arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MF, arbA, MA, functionAToBArbitrary(arbA), EQ, unsafeRun)

  fun <F, A> laws(MF: MonadFilter<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MF, applicativeArbitrary(arbA, MF), arbA, MA, arbAToA, arbAToA, EQ, unsafeRun)

  fun <F, A> laws(MF: MonadFilter<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MF, arbF, arbA, MA, arbAtoA, arbAtoA, EQ, unsafeRun)

  fun <F, A, B> laws(MF: MonadFilter<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MF, applicativeArbitrary(arbA, MF), arbA, MA, arbAToB, arbBToA, EQ, unsafeRun)


  fun <F, A, B> laws(MF: MonadFilter<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> =
    MonadLaws.laws(MF, arbF, arbA, MA, arbAToB, arbBToA, EQ, unsafeRun) + FunctorFilterLaws.laws(MF, arbF, arbA, arbAToB, arbBToA, EQ) + listOf(
      Law("MonadFilter Laws: Left Empty", MF.monadFilterLeftEmpty(arbF, EQ)),
      Law("MonadFilter Laws: Right Empty", MF.monadFilterRightEmpty(arbF, EQ)),
      Law("MonadFilter Laws: Consistency", MF.monadFilterConsistency(arbF, EQ)),
      Law("MonadFilter Laws: Comprehension Guards", MF.monadFilterEmptyComprehensions(arbA, EQ)),
      Law("MonadFilter Laws: Comprehension bindWithFilter Guards", MF.monadFilterBindWithFilterComprehensions(arbA, EQ))
    )

  fun <F, A> MonadFilter<F>.monadFilterLeftEmpty(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(
      functionAToBArbitrary<A, Kind<F, A>>(arbF)
    ) { f ->
      empty<A>().flatMap(f).eqv(empty(), EQ)
    }

  fun <F, A> MonadFilter<F>.monadFilterRightEmpty(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      fa.flatMap { empty<A>() }.eqv(empty(), EQ)
    }

  fun <F, A> MonadFilter<F>.monadFilterConsistency(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<A, Boolean>(Boolean.arbitrary()),
      arbF
    )) { (f, fa) ->
      fa.filter(f).eqv(fa.flatMap { a -> if (f(a)) just(a) else empty() }, EQ)
    }

  fun <F, A> MonadFilter<F>.monadFilterEmptyComprehensions(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(Boolean.arbitrary(), arbA)) { (guard, n) ->
      bindingFilter {
        continueIf(guard)
        n
      }.eqv(if (!guard) empty() else just(n), EQ)
    }

  fun <F, A> MonadFilter<F>.monadFilterBindWithFilterComprehensions(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(Boolean.arbitrary(), arbA)) { (guard, n) ->
      bindingFilter {
        val x = just(n).bindWithFilter { _ -> guard }
        x
      }.eqv(if (!guard) empty() else just(n), EQ)
    }
}
