package arrow.laws.laws

import arrow.Kind
import arrow.core.Either
import arrow.core.Tuple2
import arrow.core.Tuple3
import arrow.core.extensions.monoid
import arrow.effects.typeclasses.Bracket
import arrow.effects.typeclasses.ExitCase
import arrow.laws.generators.*
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.arbitrary.Gen
import arrow.propCheck.arbitrary.fix
import arrow.propCheck.arbitrary.gen.applicative.applicative
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid
import java.util.concurrent.atomic.AtomicReference

object BracketLaws {

  fun <F> laws(BF: Bracket<F, Throwable>, EQ_EITHER: Eq<Kind<F, Either<Throwable, Int>>>, EQ: Eq<Kind<F, Int>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> = laws(
    BF,
    NonFatalThrowableArbitrary,
    EQ_EITHER, EQ, unsafeRun
  )

  fun <F, E> laws(BF: Bracket<F, E>, arbE: Arbitrary<E>, EQ_EITHER: Eq<Kind<F, Either<E, Int>>>, EQ: Eq<Kind<F, Int>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> = laws(
    BF,
    Int.arbitrary(),
    arbE,
    Int.monoid(),
    EQ_EITHER, EQ, unsafeRun
  )

  fun <F, A, E> laws(BF: Bracket<F, E>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> =
    laws(BF, arbA, arbE, MA, functionAToBArbitrary(arbA), EQ_EITHER, EQ, unsafeRun)

  fun <F, A, E> laws(BF: Bracket<F, E>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    MonadErrorLaws.laws(BF, applicativeErrorArbitrary(arbA, arbE, BF), arbA, arbE, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ, unsafeRun)

  fun <F, A, E> laws(BF: Bracket<F, E>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    MonadErrorLaws.laws(BF, arbF, arbA, arbE, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ, unsafeRun)

  fun <F, A, B, E> laws(BF: Bracket<F, E>, arbA: Arbitrary<A>, arbE: Arbitrary<E>, MA: Monoid<A>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ_EITHER: Eq<Kind<F, Either<E, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    MonadErrorLaws.laws(BF, applicativeErrorArbitrary(arbA, arbE, BF), arbA, arbE, MA, arbAtoB, arbBtoA, EQ_EITHER, EQ, unsafeRun)

  fun <F, A, B, E> laws(
    BF: Bracket<F, E>,
    arbF: Arbitrary<Kind<F, A>>,
    arbA: Arbitrary<A>,
    arbE: Arbitrary<E>,
    MA: Monoid<A>,
    arbAToB: Arbitrary<(A) -> B>,
    arbBToA: Arbitrary<(B) -> A>,
    EQ_EITHER: Eq<Kind<F, Either<E, A>>>,
    EQ: Eq<Kind<F, A>>,
    unsafeRun: Kind<F, Int>.() -> Int
  ): List<Law> =
    MonadErrorLaws.laws(BF, arbF, arbA, arbE, MA, arbAToB, arbBToA, EQ_EITHER, EQ, unsafeRun) + listOf(
      Law("Bracket: bracketCase with just Unit is eqv to Map", BF.bracketCaseWithJustUnitEqvMap(arbF, makeFunAtoA(arbAToB, arbBToA), EQ)),
      Law("Bracket: bracketCase with just Unit is uncancelable", BF.bracketCaseWithJustUnitIsUncancelable(arbF, EQ)),
      Law("Bracket: bracketCase failure in acquisition remains failure", BF.bracketCaseFailureInAcquisitionRemainsFailure(arbE, EQ)),
      Law("Bracket: bracket is derived from bracketCase", BF.bracketIsDerivedFromBracketCase(arbF, EQ)),
      Law("Bracket: uncancelable prevents Canceled case", BF.uncancelablePreventsCanceledCase(BF.just(Unit), BF.just(Unit), arbF, EQ)),
      Law("Bracket: acquire and release are uncancelable", BF.acquireAndReleaseAreUncancelable({ BF.just(Unit) }, arbF, EQ)),
      Law("Bracket: guarantee is derived from bracket", BF.guaranteeIsDerivedFromBracket(BF.just(Unit), arbF, EQ)),
      Law("Bracket: guaranteeCase is derived from bracketCase", BF.guaranteeCaseIsDerivedFromBracketCase({ BF.just(Unit) }, arbF, EQ)),
      Law("Bracket: bracket propagates transformer effects", BF.bracketPropagatesTransformerEffects(arbF, arbE, EQ)),
      Law("Bracket: bracket must run release task", BF.bracketMustRunReleaseTask(arbA, MA, EQ))
    )

  fun <F, A, E> Bracket<F, E>.bracketCaseWithJustUnitEqvMap(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      arbF,
      arbAToA
    )) { (fa, f) ->
      fa.bracketCase(release = { _, _ -> just<Unit>(Unit) }, use = { a -> just(f(a)) }).eqv(fa.map(f), EQ)
    }

  fun <F, A, E> Bracket<F, E>.bracketCaseWithJustUnitIsUncancelable(
    arbF: Arbitrary<Kind<F, A>>,
    EQ: Eq<Kind<F, A>>
  ): Property =
    forAll(arbF) { fa ->
      fa.bracketCase(release = { _, _ -> just<Unit>(Unit) }, use = { just(it) }).eqv(fa.uncancelable().flatMap { just(it) }, EQ)
    }

  fun <F, A, E> Bracket<F, E>.bracketCaseFailureInAcquisitionRemainsFailure(
    arbE: Arbitrary<E>,
    EQ: Eq<Kind<F, A>>
  ): Property =
    forAll(arbE) { e ->
      raiseError<A>(e).bracketCase(release = { _, _ -> just<Unit>(Unit) }, use = { just(it) }).eqv(raiseError(e), EQ)
    }

  fun <F, A, E> Bracket<F, E>.bracketIsDerivedFromBracketCase(
    arbF: Arbitrary<Kind<F, A>>,
    EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      fa.bracket(release = { just<Unit>(Unit) }, use = { just(it) }).eqv(fa.bracketCase(release = { _, _ -> just<Unit>(Unit) }, use = { just(it) }), EQ)
    }

  fun <F, A, E> Bracket<F, E>.uncancelablePreventsCanceledCase(
    onCancel: Kind<F, Unit>,
    onFinish: Kind<F, Unit>,
    arbF: Arbitrary<Kind<F, A>>,
    EQ: Eq<Kind<F, A>>
  ): Property =
    forAll(arbF) { fa ->
      just(Unit).bracketCase(use = { fa }, release = { _, b ->
        if (b == ExitCase.Canceled) onCancel else onFinish
      }).uncancelable().eqv(fa.guarantee(onFinish), EQ)
    }

  fun <F, A, E> Bracket<F, E>.acquireAndReleaseAreUncancelable(
    release: (A) -> Kind<F, Unit>,
    arbF: Arbitrary<Kind<F, A>>,
    EQ: Eq<Kind<F, A>>
  ): Property =
    forAll(arbF) { fa ->
      fa.uncancelable().bracket({ a -> release(a).uncancelable() }) { just(it) }.eqv(fa.bracket(release) { just(it) }, EQ)
    }

  fun <F, A, E> Bracket<F, E>.guaranteeIsDerivedFromBracket(
    finalizer: Kind<F, Unit>,
    arbF: Arbitrary<Kind<F, A>>,
    EQ: Eq<Kind<F, A>>
  ): Property =
    forAll(arbF) { fa ->
      fa.guarantee(finalizer).eqv(just(Unit).bracket({ finalizer }, use = { fa }), EQ)
    }

  fun <F, A, E> Bracket<F, E>.guaranteeCaseIsDerivedFromBracketCase(
    finalizer: (ExitCase<E>) -> Kind<F, Unit>,
    arbF: Arbitrary<Kind<F, A>>,
    EQ: Eq<Kind<F, A>>
  ): Property =
    forAll(arbF) { fa ->
      fa.guaranteeCase(finalizer).eqv(just(Unit).bracketCase({ _, e -> finalizer(e) }) { fa }, EQ)
    }

  fun <F, A, E> Bracket<F, E>.bracketPropagatesTransformerEffects(arbF: Arbitrary<Kind<F, A>>, arbE: Arbitrary<E>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple3.arbitrary(
      arbF,
      functionAToBArbitrary<A, Kind<F, A>>(arbF),
      functionAToBArbitrary<A, Kind<F, Unit>>(applicativeArbitrary(Arbitrary(Gen.applicative().unit().fix()), this))
    )) { (acquire, use, release) ->
      acquire.bracket(use = use, release = release).eqv(
        acquire.flatMap { a -> use(a).flatMap { b -> release(a).map { b } } },
        EQ
      )
    }

  fun <F, A, E> Bracket<F, E>.bracketMustRunReleaseTask(arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbA) { i ->
      val msg: AtomicReference<A> = AtomicReference(MA.empty())
      just(i).bracket<A, A>(
        release = { ii -> msg.set(ii); unit() },
        use = { throw Throwable("Boom!") }
      )
        .attempt()
        .map { msg.get() }
        .eqv(just(i), EQ)
    }

}
