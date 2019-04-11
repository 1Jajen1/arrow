package arrow.laws.laws

import arrow.Kind
import arrow.core.extensions.monoid
import arrow.laws.generators.applicativeArbitrary
import arrow.laws.generators.functionAToBArbitrary
import arrow.laws.generators.makeFunAtoA
import arrow.mtl.typeclasses.MonadCombine
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.instances.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid

object MonadCombineLaws {

  fun <F> laws(MF: MonadCombine<F>, EQ: Eq<Kind<F, Int>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> = laws(
    MF,
    Int.arbitrary(),
    Int.monoid(),
    functionAToBArbitrary(Int.arbitrary()),
    EQ, unsafeRun
  )

  fun <F, A> laws(MF: MonadCombine<F>, arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> =
    laws(MF, arbA, MA, functionAToBArbitrary<A, A>(arbA), EQ, unsafeRun)

  fun <F, A> laws(MF: MonadCombine<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MF, applicativeArbitrary(arbA, MF), arbA, MA, arbAToA, arbAToA, EQ, unsafeRun)

  fun <F, A> laws(MF: MonadCombine<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MF, arbF, arbA, MA, arbAtoA, arbAtoA, EQ, unsafeRun)

  fun <F, A, B> laws(MF: MonadCombine<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MF, applicativeArbitrary(arbA, MF), arbA, MA, arbAToB, arbBToA, EQ, unsafeRun)


  fun <F, A, B> laws(MF: MonadCombine<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> =
    MonadFilterLaws.laws(MF, arbF, arbA, MA, arbAToB, arbBToA, EQ, unsafeRun) + AlternativeLaws.laws(MF, arbF, makeFunAtoA(arbAToB, arbBToA), arbA, MA, EQ)
}
