package arrow.test.laws

import arrow.Kind
import arrow.core.Either
import arrow.core.Tuple2
import arrow.core.Tuple3
import arrow.core.extensions.monoid
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.test.generators.NonFatalThrowableArbitrary
import arrow.test.generators.applicativeErrorArbitrary
import arrow.test.generators.functionAToBArbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.MonadError
import arrow.typeclasses.Monoid

object MonadErrorLaws {

  fun <F> laws(AP: MonadError<F, Throwable>, EQ_EITHER: Eq<Kind<F, Either<Throwable, Int>>>, EQ: Eq<Kind<F, Int>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> = laws(
    AP,
    NonFatalThrowableArbitrary,
    EQ_EITHER, EQ, unsafeRun
  )

  fun <F, E> laws(AP: MonadError<F, E>, arbE: Arbitrary<E>, EQ_EITHER: Eq<Kind<F, Either<E, Int>>>, EQ: Eq<Kind<F, Int>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> = laws(
    AP,
    Int.arbitrary(),
    arbE,
    Int.monoid(),
    functionAToBArbitrary(Int.arbitrary()),
    EQ_EITHER, EQ, unsafeRun
  )

  fun <F, A, E> laws(AP: MonadError<F, E>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(AP, applicativeErrorArbitrary(arbA, arbE, AP), arbA, arbE, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ, unsafeRun)

  fun <F, A, E> laws(AP: MonadError<F, E>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(AP, arbF, arbA, arbE, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ, unsafeRun)

  fun <F, A, B, E> laws(AP: MonadError<F, E>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(AP, applicativeErrorArbitrary(arbA, arbE, AP), arbA, arbE, MA, arbAtoB, arbBtoA, EQ_EITHER, EQ, unsafeRun)

  fun <F, A, B, E> laws(M: MonadError<F, E>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> =
    MonadLaws.laws(M, arbF, arbA, MA, arbAToB, arbBToA, EQ, unsafeRun) + ApplicativeErrorLaws.laws(M, arbF, arbA, arbE, MA, arbAToB, arbBToA, EQ_EITHER, EQ) + listOf(
      Law("Monad Error Laws: left zero", M.monadErrorLeftZero(arbF, arbE, EQ)),
      Law("Monad Error Laws: ensure consistency", M.monadErrorEnsureConsistency(arbF, arbE, EQ))
    )

  fun <F, A, E> MonadError<F, E>.monadErrorLeftZero(arbF: Arbitrary<Kind<F, A>>, arbE: Arbitrary<E>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<A, Kind<F, A>>(arbF),
      arbE
    )) { (f, e) ->
      raiseError<A>(e).flatMap(f).eqv(raiseError(e), EQ)
    }

  fun <F, A, E> MonadError<F, E>.monadErrorEnsureConsistency(arbF: Arbitrary<Kind<F, A>>, arbE: Arbitrary<E>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple3.arbitrary(
      arbF,
      arbE,
      functionAToBArbitrary<A, Boolean>(Boolean.arbitrary())
    )) { (fa, e, p) ->
      fa.ensure({ e }, p).eqv(fa.flatMap { a -> if (p(a)) just(a) else raiseError(e) }, EQ)
    }
}
