package arrow.laws.laws

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.monoid
import arrow.data.extensions.list.foldable.fold
import arrow.data.extensions.list.foldable.foldLeft
import arrow.data.k
import arrow.effects.data.internal.BindingCancellationException
import arrow.effects.typeclasses.MonadDefer
import arrow.laws.concurrency.SideEffect
import arrow.laws.generators.NonFatalThrowableArbitrary
import arrow.laws.generators.applicativeErrorArbitrary
import arrow.laws.generators.functionAToBArbitrary
import arrow.laws.generators.makeFunAtoA
import arrow.propCheck.Property
import arrow.propCheck.and
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.newSingleThreadContext

object MonadDeferLaws {

  fun <F> laws(SC: MonadDefer<F>, EQ_EITHER: Eq<Kind<F, Either<Throwable, Int>>>, EQ: Eq<Kind<F, Int>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true): List<Law> = laws(
    SC,
    Int.arbitrary(),
    Int.monoid(),
    functionAToBArbitrary(Int.arbitrary()),
    EQ_EITHER, EQ, unsafeRun, testStackSafety
  )

  fun <F, A> laws(SC: MonadDefer<F>, arbA: Arbitrary<A>, MA: Monoid<A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true) =
    laws(SC, arbA, MA, functionAToBArbitrary(arbA), EQ_EITHER, EQ, unsafeRun, testStackSafety)

  fun <F, A> laws(SC: MonadDefer<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true) =
    laws(SC, applicativeErrorArbitrary(arbA, NonFatalThrowableArbitrary, SC), arbA, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ, unsafeRun, testStackSafety)

  fun <F, A> laws(SC: MonadDefer<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true) =
    laws(SC, arbF, arbA, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ, unsafeRun, testStackSafety)

  fun <F, A, B> laws(SC: MonadDefer<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true) =
    laws(SC, applicativeErrorArbitrary(arbA, NonFatalThrowableArbitrary, SC), arbA, MA, arbAtoB, arbBtoA, EQ_EITHER, EQ, unsafeRun, testStackSafety)

  fun <F, A, B> laws(
    SC: MonadDefer<F>,
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
    BracketLaws.laws(SC, arbF, arbA, NonFatalThrowableArbitrary, MA, arbAToB, arbBToA, EQ_EITHER, EQ, unsafeRun) + listOf(
      Law("MonadDefer bind: binding blocks", SC.asyncBind(arbA, makeFunAtoA(arbAToB, arbBToA), EQ)),
      Law("MonadDefer bind: binding failure", SC.asyncBindError(EQ)),
      Law("MonadDefer bind: unsafe binding", SC.asyncBindUnsafe(arbA, makeFunAtoA(arbAToB, arbBToA), EQ)),
      Law("MonadDefer bind: unsafe binding failure", SC.asyncBindUnsafeError(EQ)),
      Law("MonadDefer bind: binding in parallel", SC.asyncParallelBind(arbA, MA, EQ)),
      Law("MonadDefer bind: binding cancellation before flatMap", SC.asyncCancellationBefore(arbA, makeFunAtoA(arbAToB, arbBToA), EQ)),
      Law("MonadDefer bind: binding cancellation after flatMap", SC.asyncCancellationAfter(arbA, makeFunAtoA(arbAToB, arbBToA), EQ)),
      Law("MonadDefer bind: bindingInContext cancellation before flatMap", SC.inContextCancellationBefore(arbA, makeFunAtoA(arbAToB, arbBToA), EQ)),
      Law("MonadDefer bind: bindingInContext cancellation after flatMap", SC.inContextCancellationAfter(arbA, makeFunAtoA(arbAToB, arbBToA), EQ)),
      Law("MonadDefer bind: bindingInContext throw equivalent to raiseError", SC.inContextErrorThrow(EQ)),
      Law("MonadDefer bind: monad comprehensions binding in other threads equivalence", SC.monadComprehensionsBindInContextEquivalent(arbA, makeFunAtoA(arbAToB, arbBToA), EQ)),
      Law("MonadDefer laws: delay constant equals pure", SC.delayConstantEqualsPure(arbA, EQ)),
      Law("MonadDefer laws: delay throw equals raiseError", SC.delayThrowEqualsRaiseError(EQ)),
      Law("MonadDefer laws: defer constant equals pure", SC.deferConstantEqualsPure(arbA, EQ)),
      Law("MonadDefer laws: delayOrRaise constant right equals pure", SC.delayOrRaiseConstantRightEqualsPure(arbA, EQ)),
      Law("MonadDefer laws: delayOrRaise constant left equals raiseError", SC.delayOrRaiseConstantLeftEqualsRaiseError(EQ)),
      Law("MonadDefer laws: propagate error through bind", SC.propagateErrorsThroughBind(EQ)),
      Law("MonadDefer laws: defer suspens evaluation", SC.deferSuspendsEvaluation(unsafeRun)),
      Law("MonadDefer laws: delay suspends evaluation", SC.delaySuspendsEvaluation(unsafeRun)),
      Law("MonadDefer laws: flatMap suspends evaluation", SC.flatMapSuspendsEvaluation(unsafeRun)),
      Law("MonadDefer laws: map suspends evaluation", SC.mapSuspendsEvaluation(unsafeRun)),
      Law("MonadDefer laws: Repeated evaluation not memoized", SC.repeatedSyncEvaluationNotMemoized(unsafeRun))
    ) + if (testStackSafety) {
      listOf(
        Law("MonadDefer laws: stack safety over repeated left binds", SC.stackSafetyOverRepeatedLeftBinds(5000, unsafeRun)),
        Law("MonadDefer laws: stack safety over repeated right binds", SC.stackSafetyOverRepeatedRightBinds(5000, unsafeRun)),
        Law("MonadDefer laws: stack safety over repeated attempts", SC.stackSafetyOverRepeatedAttempts(5000, unsafeRun)),
        Law("MonadDefer laws: stack safety over repeated maps", SC.stackSafetyOnRepeatedMaps(5000, unsafeRun))
      )
    } else {
      emptyList()
    }

  fun <F, A> MonadDefer<F>.delayConstantEqualsPure(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbA) { x ->
      delay { x }.eqv(just(x), EQ)
    }

  fun <F, A> MonadDefer<F>.deferConstantEqualsPure(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbA) { x ->
      defer { just(x) }.eqv(just(x), EQ)
    }

  fun <F, A> MonadDefer<F>.delayOrRaiseConstantRightEqualsPure(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbA) { x ->
      delayOrRaise { x.right() }.eqv(just(x), EQ)
    }

  fun <F, A> MonadDefer<F>.delayOrRaiseConstantLeftEqualsRaiseError(EQ: Eq<Kind<F, A>>): Property =
    forAll(NonFatalThrowableArbitrary) { t ->
      delayOrRaise { t.left() }.eqv(raiseError(t), EQ)
    }

  fun <F, A> MonadDefer<F>.delayThrowEqualsRaiseError(EQ: Eq<Kind<F, A>>): Property =
    forAll(NonFatalThrowableArbitrary) { t ->
      delay { throw t }.eqv(raiseError(t), EQ)
    }

  fun <F, A> MonadDefer<F>.propagateErrorsThroughBind(EQERR: Eq<Kind<F, A>>): Property =
    forAll(NonFatalThrowableArbitrary) { t ->
      delay { throw t }.flatMap<A, A> { a: A -> just(a) }.eqv(raiseError(t), EQERR)
    }

  fun <F> MonadDefer<F>.deferSuspendsEvaluation(unsafeRun: Kind<F, Int>.() -> Int): Property {
    val sideEffect = SideEffect(counter = 0)
    val df = defer { sideEffect.increment(); just(sideEffect.counter) }

    return sideEffect.counter.eqv(0) and df.unsafeRun().eqv(1)
  }

  fun <F> MonadDefer<F>.delaySuspendsEvaluation(unsafeRun: Kind<F, Int>.() -> Int): Property {
    val sideEffect = SideEffect(counter = 0)
    val df = delay { sideEffect.increment(); sideEffect.counter }

    return sideEffect.counter.eqv(0) and df.unsafeRun().eqv(1)
  }

  fun <F> MonadDefer<F>.flatMapSuspendsEvaluation(unsafeRun: Kind<F, Int>.() -> Int): Property {
    val sideEffect = SideEffect(counter = 0)
    val df = just(0).flatMap { sideEffect.increment(); just(sideEffect.counter) }

    return sideEffect.counter.eqv(0) and df.unsafeRun().eqv(1)
  }

  fun <F> MonadDefer<F>.mapSuspendsEvaluation(unsafeRun: Kind<F, Int>.() -> Int): Property {
    val sideEffect = SideEffect(counter = 0)
    val df = just(0).map { sideEffect.increment(); sideEffect.counter }

    return sideEffect.counter.eqv(0) and df.unsafeRun().eqv(1)
  }

  fun <F> MonadDefer<F>.repeatedSyncEvaluationNotMemoized(unsafeRun: Kind<F, Int>.() -> Int): Property {
    val sideEffect = SideEffect()
    val df = delay { sideEffect.increment(); sideEffect.counter }

    return df.flatMap { df }.flatMap { df }.unsafeRun().eqv(3)
  }

  fun <F> MonadDefer<F>.stackSafetyOverRepeatedLeftBinds(iterations: Int = 5000, unsafeRun: Kind<F, Int>.() -> Int): Property =
    (0..iterations).toList().k().foldLeft(just(0)) { def, x ->
      def.flatMap { just(x) }
    }.unsafeRun().eqv(iterations)

  fun <F> MonadDefer<F>.stackSafetyOverRepeatedRightBinds(iterations: Int = 5000, unsafeRun: Kind<F, Int>.() -> Int): Property =
    (0..iterations).toList().foldRight(just(iterations)) { x, def ->
      lazy().flatMap { def }
    }.unsafeRun().eqv(iterations)

  fun <F> MonadDefer<F>.stackSafetyOverRepeatedAttempts(iterations: Int = 5000, unsafeRun: Kind<F, Int>.() -> Int): Property =
    (0..iterations).toList().foldLeft(just(0)) { def, x ->
      def.attempt().map { x }
    }.unsafeRun().eqv(iterations)

  fun <F> MonadDefer<F>.stackSafetyOnRepeatedMaps(iterations: Int = 5000, unsafeRun: Kind<F, Int>.() -> Int): Property =
    (0..iterations).toList().foldLeft(just(0)) { def, x ->
      def.map { x }
    }.unsafeRun().eqv(iterations)

  fun <F, A> MonadDefer<F>.asyncBind(arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple3.arbitrary(
      arbA,
      arbAToA,
      arbAToA
    )) { (x, f, g) ->
      val (bound, _) = bindingCancellable {
        val a = bindDefer { x }
        val b = bindDefer { f(a) }
        val c = bindDefer { g(b) }
        c
      }
      bound.eqv(just(g(f(x))), EQ)
    }

  fun <F, A> MonadDefer<F>.asyncBindError(EQ: Eq<Kind<F, A>>): Property =
    forAll(NonFatalThrowableArbitrary) { e: Throwable ->
      val (bound: Kind<F, A>, _) = bindingCancellable<A> {
        bindDefer { throw e }
      }
      bound.eqv(raiseError(e), EQ)
    }

  fun <F, A> MonadDefer<F>.asyncBindUnsafe(arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple3.arbitrary(
      arbA,
      arbAToA,
      arbAToA
    )) { (x, f, g) ->
      val (bound, _) = bindingCancellable {
        val a = bindDelayOrRaise { Right(x) }
        val b = bindDelayOrRaise { Right(f(a)) }
        val c = bindDelayOrRaise { Right(g(b)) }
        c
      }
      bound.eqv(just(g(f(x))), EQ)
    }

  fun <F, A> MonadDefer<F>.asyncBindUnsafeError(EQ: Eq<Kind<F, A>>): Property =
    forAll(NonFatalThrowableArbitrary) { e: Throwable ->
      val (bound: Kind<F, A>, _) = bindingCancellable<A> {
        bindDelayOrRaise { Left(e) }
      }
      bound.eqv(raiseError(e), EQ)
    }

  fun <F, A> MonadDefer<F>.asyncParallelBind(arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple3.arbitrary(
      arbA,
      arbA,
      arbA
    )) { (x, y, z) ->
      val (bound, _) = bindingCancellable {
        val value = bind { tupled(delay { x }, delay { y }, delay { z }) }
        listOf(value.a, value.b, value.c).fold(MA)
      }
      bound.eqv(just(listOf(x, y, z).fold(MA)), EQ)
    }

  fun <F, A> MonadDefer<F>.asyncCancellationBefore(arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple3.arbitrary(
      arbA,
      arbAToA,
      arbAToA
    )) { (a, f, g) ->
      val sideEffect = SideEffect()
      val (binding, dispose) = bindingCancellable {
        val a = bindDefer { Thread.sleep(20); a }
        sideEffect.increment()
        val b = bindDefer { f(a) }
        val (c) = just(g(b))
        c
      }
      Try { Thread.sleep(10); dispose() }.recover { throw it }
      binding.eqv(raiseError(BindingCancellationException()), EQ) and sideEffect.counter.eqv(0)
    }

  fun <F, A> MonadDefer<F>.asyncCancellationAfter(arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      arbA,
      arbAToA
    )) { (x, f) ->
      val sideEffect = SideEffect()
      val (binding, dispose) = bindingCancellable {
        val a = bindDefer { x }
        sideEffect.increment()
        val b = bindDefer { Thread.sleep(20); sideEffect.increment(); f(a) }
        b
      }
      Try { Thread.sleep(10); dispose() }.recover { throw it }
      binding.eqv(raiseError(BindingCancellationException()), EQ) and sideEffect.counter.eqv(0)
    }

  fun <F, A> MonadDefer<F>.inContextCancellationBefore(arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple3.arbitrary(
      arbA,
      arbAToA,
      arbAToA
    )) { (x, f, g) ->
      val sideEffect = SideEffect()
      val (binding, dispose) = bindingCancellable {
        val a = bindIn(Dispatchers.Default) { Thread.sleep(20); x }
        sideEffect.increment()
        val b = bindIn(Dispatchers.Default) { f(a) }
        val (c) = just(g(b))
        c
      }
      Try { Thread.sleep(10); dispose() }.recover { throw it }
      binding.eqv(raiseError(BindingCancellationException()), EQ) and sideEffect.counter.eqv(0)
    }

  fun <F, A> MonadDefer<F>.inContextCancellationAfter(arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      arbA,
      arbAToA
    )) { (x, f) ->
      val sideEffect = SideEffect()
      val (binding, dispose) = bindingCancellable {
        val a = bindIn(Dispatchers.Default) { x }
        sideEffect.increment()
        val b = bindIn(Dispatchers.Default) { Thread.sleep(20); sideEffect.increment(); f(a) }
        b
      }
      Try { Thread.sleep(10); dispose() }.recover { throw it }
      binding.eqv(raiseError(BindingCancellationException()), EQ) and sideEffect.counter.eqv(0)
    }

  @Suppress("UNREACHABLE_CODE")
  fun <F, A> MonadDefer<F>.inContextErrorThrow(EQ: Eq<Kind<F, A>>): Property =
    forAll(NonFatalThrowableArbitrary) { throwable: Throwable ->
      bindingCancellable {
        bindIn(newSingleThreadContext("1")) { throw throwable }
      }.a.eqv(raiseError(throwable), EQ)
    }

  fun <F, A> MonadDefer<F>.monadComprehensionsBindInContextEquivalent(arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      arbA,
      arbAToA
    )) { (x, f) ->
      val bindM = bindingCancellable {
        val a = bindDeferIn(newSingleThreadContext("1")) { x }
        val b = bindDeferIn(newSingleThreadContext("2")) { f(a) }
        b
      }
      val bind = bindingCancellable {
        val a = bindIn(newSingleThreadContext("$1")) { x }
        val b = bindIn(newSingleThreadContext("2")) { f(a) }
        b
      }
      bindM.a.eqv(bind.a, EQ)
    }
}
