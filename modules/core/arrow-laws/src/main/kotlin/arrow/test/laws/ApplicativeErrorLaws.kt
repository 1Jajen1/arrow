package arrow.test.laws

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.monoid
import arrow.propCheck.*
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.either.arbitrary.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.testresult.testable.testable
import arrow.test.generators.*
import arrow.typeclasses.ApplicativeError
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid

object ApplicativeErrorLaws {

  fun <F> laws(AP: ApplicativeError<F, Throwable>, EQ_EITHER: Eq<Kind<F, Either<Throwable, Int>>>, EQ: Eq<Kind<F, Int>>): List<Law> = laws(
    AP,
    NonFatalThrowableArbitrary,
    EQ_EITHER, EQ
  )

  fun <F, E> laws(AP: ApplicativeError<F, E>, arbE: Arbitrary<E>, EQ_EITHER: Eq<Kind<F, Either<E, Int>>>, EQ: Eq<Kind<F, Int>>): List<Law> = laws(
    AP,
    applicativeErrorArbitrary(Int.arbitrary(), arbE, AP),
    Int.arbitrary(),
    arbE,
    Int.monoid(),
    functionAToBArbitrary(Int.arbitrary()),
    EQ_EITHER, EQ
  )

  fun <F, A, E> laws(AP: ApplicativeError<F, E>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>) =
    laws(AP, applicativeErrorArbitrary(arbA, arbE, AP), arbA, arbE, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ)

  fun <F, A, E> laws(AP: ApplicativeError<F, E>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>) =
    laws(AP, arbF, arbA, arbE, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ)

  fun <F, A, B, E> laws(AP: ApplicativeError<F, E>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>) =
    laws(AP, applicativeErrorArbitrary(arbA, arbE, AP), arbA, arbE, MA, arbAtoB, arbBtoA, EQ_EITHER, EQ)

  fun <F, A, B, E> laws(AE: ApplicativeError<F, E>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>): List<Law> =
    ApplicativeLaws.laws(AE, arbF, arbA, MA, arbAToB, arbBToA, EQ) + listOf(
      Law("Applicative Error Laws: handle", AE.applicativeErrorHandle(arbA, arbE, EQ)),
      Law("Applicative Error Laws: handle with for error", AE.applicativeErrorHandleWith(arbF, arbE, EQ)),
      Law("Applicative Error Laws: handle with for success", AE.applicativeErrorHandleWithPure(arbA, EQ)),
      Law("Applicative Error Laws: attempt for error", AE.applicativeErrorAttemptError(arbE, EQ_EITHER)),
      Law("Applicative Error Laws: attempt for success", AE.applicativeErrorAttemptSuccess(arbA, EQ_EITHER)),
      Law("Applicative Error Laws: attempt lift from Either consistent with pure", AE.applicativeErrorAttemptFromEitherConsistentWithPure(arbA, arbE, EQ_EITHER)),
      Law("Applicative Error Laws: catch captures errors", AE.applicativeErrorCatch(arbA, arbE, EQ))
    )

  fun <F, A, E> ApplicativeError<F, E>.applicativeErrorHandle(arbA: Arbitrary<A>, arbE: Arbitrary<E>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<E, A>(arbA),
      arbE
    )) { (f, e) ->
      raiseError<A>(e).handleError(f).eqv(just(f(e)), EQ)
    }

  fun <F, A, E> ApplicativeError<F, E>.applicativeErrorHandleWith(arbF: Arbitrary<Kind<F, A>>, arbE: Arbitrary<E>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<E, Kind<F, A>>(arbF),
      arbE
    )) { (f, e) ->
      raiseError<A>(e).handleErrorWith(f).eqv(f(e), EQ)
    }

  fun <F, A, E> ApplicativeError<F, E>.applicativeErrorHandleWithPure(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<E, Kind<F, A>>(applicativeArbitrary(arbA, this)),
      arbA
    )) { (f, a) ->
      just(a).handleErrorWith(f).eqv(just(a), EQ)
    }

  fun <F, A, E> ApplicativeError<F, E>.applicativeErrorAttemptError(arbE: Arbitrary<E>, EQ: Eq<Kind<F, Either<E, A>>>): Property =
    forAll(arbE) { e ->
      raiseError<A>(e).attempt().eqv(just(Left(e)), EQ)
    }

  fun <F, A, E> ApplicativeError<F, E>.applicativeErrorAttemptSuccess(arbA: Arbitrary<A>, EQ: Eq<Kind<F, Either<E, A>>>): Property =
    forAll(arbA) { a ->
      just(a).attempt().eqv(just(Right(a)), EQ)
    }

  fun <F, A, E> ApplicativeError<F, E>.applicativeErrorAttemptFromEitherConsistentWithPure(arbA: Arbitrary<A>, arbE: Arbitrary<E>, EQ: Eq<Kind<F, Either<E, A>>>): Property =
    forAll(Either.arbitrary(arbE, arbA)) { either ->
      either.fromEither { it }.attempt().eqv(just(either), EQ)
    }

  fun <F, A, E> ApplicativeError<F, E>.applicativeErrorCatch(arbA: Arbitrary<A>, arbE: Arbitrary<E>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      Either.arbitrary(NonFatalThrowableArbitrary, arbA),
      functionAToBArbitrary<Throwable, E>(arbE)
    )) { (either, f) ->
      catch(f) { either.fold({ throw it }, ::identity) }.eqv(either.fold({ raiseError<A>(f(it)) }, { just(it) }), EQ)
    }

  fun <F, A, E> ApplicativeError<F, E>.applicativeErrorThrowsFatalThrowables(arbE: Arbitrary<E>): Property =
    forAll(Tuple2.arbitrary(
      FatalThrowableArbitrary,
      functionAToBArbitrary<Throwable, E>(arbE)
    )) { (fatal, f) ->
      try {
        catch(f) { throw fatal }
        TestResult.testable().run { failed("Should have thrown").property() }
      } catch (e: Throwable) {
        e.eqv(fatal)
      }
    }

}
