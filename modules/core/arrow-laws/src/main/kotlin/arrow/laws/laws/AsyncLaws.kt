package arrow.laws.laws

import arrow.Kind
import arrow.core.Either
import arrow.core.Left
import arrow.core.Right
import arrow.core.Tuple2
import arrow.core.extensions.monoid
import arrow.effects.Promise
import arrow.effects.typeclasses.Async
import arrow.effects.typeclasses.ExitCase
import arrow.laws.generators.NonFatalThrowableArbitrary
import arrow.laws.generators.ThrowableArbitrary
import arrow.laws.generators.applicativeErrorArbitrary
import arrow.laws.generators.functionAToBArbitrary
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.either.arbitrary.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid
import kotlinx.coroutines.newSingleThreadContext

object AsyncLaws {

  fun <F> laws(AC: Async<F>, EQ_EITHER: Eq<Kind<F, Either<Throwable, Int>>>, EQ: Eq<Kind<F, Int>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true): List<Law> = laws(
    AC,
    Int.arbitrary(),
    Int.monoid(),
    EQ_EITHER, EQ, unsafeRun, testStackSafety
  )

  fun <F, A> laws(AC: Async<F>, arbA: Arbitrary<A>, MA: Monoid<A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true): List<Law> =
    laws(AC, arbA, MA, functionAToBArbitrary(arbA), EQ_EITHER, EQ, unsafeRun, testStackSafety)

  fun <F, A> laws(AC: Async<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true) =
    laws(AC, applicativeErrorArbitrary(arbA, NonFatalThrowableArbitrary, AC), arbA, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ, unsafeRun, testStackSafety)

  fun <F, A> laws(AC: Async<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true) =
    laws(AC, arbF, arbA, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ, unsafeRun, testStackSafety)

  fun <F, A, B> laws(AC: Async<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true) =
    laws(AC, applicativeErrorArbitrary(arbA, NonFatalThrowableArbitrary, AC), arbA, MA, arbAtoB, arbBtoA, EQ_EITHER, EQ, unsafeRun, testStackSafety)

  fun <F, A, B> laws(
    AC: Async<F>,
    arbF: Arbitrary<Kind<F, A>>,
    arbA: Arbitrary<A>,
    MA: Monoid<A>,
    arbAToB: Arbitrary<(A) -> B>,
    arbBToA: Arbitrary<(B) -> A>,
    EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>,
    EQ: Eq<Kind<F, A>>,
    unsafeRun: Kind<F, Int>.() -> Int,
    testStackSafety: Boolean = true
  ): List<Law> =
    MonadDeferLaws.laws(AC, arbF, arbA, MA, arbAToB, arbBToA, EQ_EITHER, EQ, unsafeRun, testStackSafety) + listOf(
      Law("Async Laws: success equivalence", AC.asyncSuccess(arbA, EQ)),
      Law("Async Laws: error equivalence", AC.asyncError(EQ)),
      Law("Async Laws: continueOn jumps threads", AC.continueOn(unsafeRun)),
      Law("Async Laws: async constructor", AC.asyncConstructor(unsafeRun)),
      Law("Async Laws: async can be derived from asyncF", AC.asyncCanBeDerivedFromAsyncF(arbA, EQ)),
      Law("Async Laws: bracket release is called on completed or error", AC.bracketReleaseIscalledOnCompletedOrError(arbF, arbA, EQ)),
      Law("Async Laws: continueOn on comprehensions", AC.continueOnComprehension(unsafeRun))
    )

  fun <F, A> Async<F>.asyncSuccess(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbA) { num ->
      async { ff: (Either<Throwable, A>) -> Unit -> ff(Right(num)) }.eqv(just(num), EQ)
    }

  fun <F, A> Async<F>.asyncError(EQ: Eq<Kind<F, A>>): Property =
    forAll(NonFatalThrowableArbitrary) { e ->
      async { ff: (Either<Throwable, A>) -> Unit -> ff(Left(e)) }.eqv(raiseError(e), EQ)
    }

  fun <F> Async<F>.continueOn(unsafeRun: Kind<F, Int>.() -> Int): Property =
    forAll(Tuple2.arbitrary(
      Int.arbitrary(),
      Int.arbitrary()
    )) { (threadId1, threadId2) ->
      Unit.just()
        .continueOn(newSingleThreadContext(threadId1.toString()))
        .map { getCurrentThread() }
        .continueOn(newSingleThreadContext(threadId2.toString()))
        .map { it + getCurrentThread() }
        .unsafeRun()
        .eqv(threadId1 + threadId2)
    }

  fun <F> Async<F>.asyncConstructor(unsafeRun: Kind<F, Int>.() -> Int): Property =
    forAll(Tuple2.arbitrary(Int.arbitrary(), Int.arbitrary())) { (threadId1, threadId2) ->
      invoke(newSingleThreadContext(threadId1.toString())) { getCurrentThread() }
        .flatMap {
          invoke(newSingleThreadContext(threadId2.toString())) { it + getCurrentThread() }
        }
        .unsafeRun()
        .eqv(threadId1 + threadId2)
    }

  fun <F> Async<F>.continueOnComprehension(unsafeRun: Kind<F, Int>.() -> Int): Property =
    forAll(Tuple2.arbitrary(Int.arbitrary(), Int.arbitrary())) { (threadId1, threadId2) ->
      bindingCancellable {
        continueOn(newSingleThreadContext(threadId1.toString()))
        val t1: Int = getCurrentThread()
        continueOn(newSingleThreadContext(threadId2.toString()))
        t1 + getCurrentThread()
      }.a.unsafeRun().eqv(threadId1 + threadId2)
    }

  fun <F, A> Async<F>.asyncCanBeDerivedFromAsyncF(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Either.arbitrary(ThrowableArbitrary, arbA)) { eith ->
      val k: ((Either<Throwable, A>) -> Unit) -> Unit = { f ->
        f(eith)
      }

      async(k).eqv(asyncF { cb -> delay { k(cb) } }, EQ)
    }

  fun <F, A> Async<F>.bracketReleaseIscalledOnCompletedOrError(arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      arbF,
      arbA
    )) { (fa, b) ->
      Promise.uncancelable<F, A>(this@bracketReleaseIscalledOnCompletedOrError).flatMap { promise ->
        val br = delay { promise }.bracketCase(use = { fa }, release = { r, exitCase ->
          when (exitCase) {
            is ExitCase.Completed -> r.complete(b)
            is ExitCase.Error -> r.complete(b)
            else -> just<Unit>(Unit)
          }
        })

        asyncF<Unit> { cb -> delay { cb(Right(Unit)) }.flatMap { br.attempt().`as`(Unit) } }
          .flatMap { promise.get() }
      }.eqv(just(b), EQ)
    }

  // Turns out that kotlinx.coroutines decides to rewrite thread names
  private fun getCurrentThread() =
    Thread.currentThread().name.substringBefore(' ').toInt()

}
