package arrow.laws.laws

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.monoid
import arrow.effects.CancelToken
import arrow.effects.MVar
import arrow.effects.Promise
import arrow.effects.Semaphore
import arrow.effects.typeclasses.Concurrent
import arrow.effects.typeclasses.ExitCase
import arrow.effects.typeclasses.fold
import arrow.laws.generators.NonFatalThrowableArbitrary
import arrow.laws.generators.ThrowableArbitrary
import arrow.laws.generators.applicativeErrorArbitrary
import arrow.laws.generators.functionAToBArbitrary
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.arbitrary.Gen
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.either.arbitrary.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.propCheck.instances.tuple4.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid
import kotlinx.coroutines.Dispatchers
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import kotlin.coroutines.CoroutineContext

@Suppress("LargeClass")
object ConcurrentLaws {

  fun <F> laws(CF: Concurrent<F>, EQ_EITHER: Eq<Kind<F, Either<Throwable, Int>>>, EQ: Eq<Kind<F, Int>>, EQ_UNIT: Eq<Kind<F, Unit>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true, ctx: CoroutineContext = Dispatchers.Default): List<Law> = laws(
    CF,
    Int.arbitrary(),
    Int.monoid(),
    EQ_EITHER, EQ, EQ_UNIT, unsafeRun, testStackSafety, ctx
  )

  fun <F, A> laws(CF: Concurrent<F>, arbA: Arbitrary<A>, MA: Monoid<A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, EQ_UNIT: Eq<Kind<F, Unit>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true, ctx: CoroutineContext = Dispatchers.Default): List<Law> =
    laws(CF, arbA, MA, functionAToBArbitrary(arbA), EQ_EITHER, EQ, EQ_UNIT, unsafeRun, testStackSafety, ctx)

  fun <F, A> laws(CF: Concurrent<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, EQ_UNIT: Eq<Kind<F, Unit>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true, ctx: CoroutineContext = Dispatchers.Default) =
    laws(CF, applicativeErrorArbitrary(arbA, NonFatalThrowableArbitrary, CF), arbA, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ, EQ_UNIT, unsafeRun, testStackSafety, ctx)

  fun <F, A> laws(CF: Concurrent<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, EQ_UNIT: Eq<Kind<F, Unit>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true, ctx: CoroutineContext = Dispatchers.Default) =
    laws(CF, arbF, arbA, MA, arbAtoA, arbAtoA, EQ_EITHER, EQ, EQ_UNIT, unsafeRun, testStackSafety, ctx)

  fun <F, A, B> laws(CF: Concurrent<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>, EQ: Eq<Kind<F, A>>, EQ_UNIT: Eq<Kind<F, Unit>>, unsafeRun: Kind<F, Int>.() -> Int, testStackSafety: Boolean = true, ctx: CoroutineContext = Dispatchers.Default) =
    laws(CF, applicativeErrorArbitrary(arbA, NonFatalThrowableArbitrary, CF), arbA, MA, arbAtoB, arbBtoA, EQ_EITHER, EQ, EQ_UNIT, unsafeRun, testStackSafety, ctx)

  fun <F, A, B> laws(
    CF: Concurrent<F>,
    arbF: Arbitrary<Kind<F, A>>,
    arbA: Arbitrary<A>,
    MA: Monoid<A>,
    arbAToB: Arbitrary<(A) -> B>,
    arbBToA: Arbitrary<(B) -> A>,
    EQ_EITHER: Eq<Kind<F, Either<Throwable, A>>>,
    EQ: Eq<Kind<F, A>>,
    EQ_UNIT: Eq<Kind<F, Unit>>,
    unsafeRun: Kind<F, Int>.() -> Int,
    testStackSafety: Boolean = true,
    ctx: CoroutineContext = Dispatchers.Default
  ): List<Law> =
    AsyncLaws.laws(CF, arbF, arbA, MA, arbAToB, arbBToA, EQ_EITHER, EQ, unsafeRun, testStackSafety) + listOf(
      Law("Concurrent Laws: cancel on bracket releases", CF.cancelOnBracketReleases(arbA, MA, EQ, ctx)),
      Law("Concurrent Laws: acquire is not cancelable", CF.acquireBracketIsNotCancelable(arbA, EQ, ctx)),
      Law("Concurrent Laws: release is not cancelable", CF.releaseBracketIsNotCancelable(arbA, EQ, ctx)),
      Law("Concurrent Laws: async cancelable coherence", CF.asyncCancelableCoherence(arbA, EQ)),
      Law("Concurrent Laws: cancelable cancelableF coherence", CF.cancelableCancelableFCoherence(arbA, EQ)),
      Law("Concurrent Laws: cancelable should run CancelToken on cancel", CF.cancelableReceivesCancelSignal(arbA, EQ, ctx)),
      Law("Concurrent Laws: cancelableF should run CancelToken on cancel", CF.cancelableFReceivesCancelSignal(arbA, EQ, ctx)),
      Law("Concurrent Laws: async can cancel upstream", CF.asyncCanCancelUpstream(arbA, EQ, ctx)),
      Law("Concurrent Laws: async should run KindConnection on Fiber#cancel", CF.asyncShouldRunKindConnectionOnCancel(arbA, EQ, ctx)),
      Law("Concurrent Laws: asyncF register can be cancelled", CF.asyncFRegisterCanBeCancelled(arbA, EQ, ctx)),
      Law("Concurrent Laws: asyncF can cancel upstream", CF.asyncFCanCancelUpstream(arbA, EQ, ctx)),
      Law("Concurrent Laws: asyncF should run KindConnection on Fiber#cancel", CF.asyncFShouldRunKindConnectionOnCancel(arbA, EQ, ctx)),
      Law("Concurrent Laws: start join is identity", CF.startJoinIsIdentity(arbF, EQ, ctx)),
      Law("Concurrent Laws: join is idempotent", CF.joinIsIdempotent(arbA, EQ, ctx)),
      Law("Concurrent Laws: start cancel is unit", CF.startCancelIsUnit(EQ_UNIT, ctx)),
      Law("Concurrent Laws: uncancelable mirrors source", CF.uncancelableMirrorsSource(arbA, EQ)),
      Law("Concurrent Laws: race pair mirrors left winner", CF.racePairMirrorsLeftWinner(arbF, EQ, ctx)),
      Law("Concurrent Laws: race pair mirrors right winner", CF.racePairMirrorsRightWinner(arbF, EQ, ctx)),
      Law("Concurrent Laws: race pair can cancel loser", CF.racePairCanCancelsLoser(arbA, EQ, ctx)),
      Law("Concurrent Laws: race pair can join left", CF.racePairCanJoinLeft(arbA, EQ, ctx)),
      Law("Concurrent Laws: race pair can join right", CF.racePairCanJoinRight(arbA, EQ, ctx)),
      Law("Concurrent Laws: cancelling race pair cancels both", CF.racePairCancelCancelsBoth(arbA, MA, EQ, ctx)),
      Law("Concurrent Laws: race pair is cancellable by participants", CF.racePairCanBeCancelledByParticipants(arbA, EQ, ctx)),
      Law("Concurrent Laws: race triple mirrors left winner", CF.raceTripleMirrorsLeftWinner(arbF, EQ, ctx)),
      Law("Concurrent Laws: race triple mirrors middle winner", CF.raceTripleMirrorsMiddleWinner(arbF, EQ, ctx)),
      Law("Concurrent Laws: race triple mirrors right winner", CF.raceTripleMirrorsRightWinner(arbF, EQ, ctx)),
      Law("Concurrent Laws: race triple can cancel loser", CF.raceTripleCanCancelsLoser(arbA, MA, EQ, ctx)),
      Law("Concurrent Laws: race triple can join left", CF.raceTripleCanJoinLeft(arbA, EQ, ctx)),
      Law("Concurrent Laws: race triple can join middle", CF.raceTripleCanJoinMiddle(arbA, EQ, ctx)),
      Law("Concurrent Laws: race triple can join right", CF.raceTripleCanJoinRight(arbA, EQ, ctx)),
      Law("Concurrent Laws: race triple is cancellable by participants", CF.raceTripleCanBeCancelledByParticipants(arbA, EQ, ctx)),
      Law("Concurrent Laws: cancelling race triple cancels all", CF.raceTripleCancelCancelsAll(arbA, MA, EQ, ctx)),
      Law("Concurrent Laws: race mirrors left winner", CF.raceMirrorsLeftWinner(arbF, EQ, ctx)),
      Law("Concurrent Laws: race mirrors right winner", CF.raceMirrorsRightWinner(arbF, EQ, ctx)),
      Law("Concurrent Laws: race cancels loser", CF.raceCancelsLoser(arbA, EQ, ctx)),
      Law("Concurrent Laws: race cancels both", CF.raceCancelCancelsBoth(arbA, MA, EQ, ctx)),
      Law("Concurrent Laws: race is cancellable by participants", CF.raceCanBeCancelledByParticipants(arbA, EQ, ctx)),
      Law("Concurrent Laws: parallel map cancels both", CF.parMapCancelCancelsBoth(arbA, MA, EQ, ctx)),
      Law("Concurrent Laws: parallel is cancellable by participants", CF.parMapCanBeCancelledByParticipants(arbA, EQ, ctx)),
      Law("Concurrent Laws: action concurrent with pure value is just action", CF.actionConcurrentWithPureValueIsJustAction(arbA, EQ, ctx))
    )

  fun <F, A> Concurrent<F>.cancelOnBracketReleases(arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      bindingCancellable {
        val startLatch = Promise<F, A>(this@cancelOnBracketReleases).bind() // A promise that `use` was executed
        val exitLatch = Promise<F, A>(this@cancelOnBracketReleases).bind() // A promise that `release` was executed

        val (_, cancel) = ctx.startFiber(just(i).bracketCase(
          use = { a -> startLatch.complete(a).flatMap { never<A>() } },
          release = { r, exitCase ->
            when (exitCase) {
              is ExitCase.Canceled -> exitLatch.complete(r) //Fulfil promise that `release` was executed with Canceled
              else -> just(Unit)
            }
          }
        )).bind() // Fork execution, allowing us to cancel it later

        val waitStart = startLatch.get().bind() //Waits on promise of `use`
        ctx.startFiber(cancel).bind() //Cancel bracketCase
        val waitExit = exitLatch.get().bind() // Observes cancellation via bracket's `release`

        MA.run { waitStart + waitExit }
      }.a.eqv(just(MA.run { i + i }), EQ)
    }

  fun <F, A> Concurrent<F>.acquireBracketIsNotCancelable(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple2.arbitrary(arbA, arbA)) { (a, b) ->
      bindingCancellable {
        val mvar = MVar(a, this@acquireBracketIsNotCancelable).bind()
        val p = Promise.uncancelable<F, Unit>(this@acquireBracketIsNotCancelable).bind()
        val task = p.complete(Unit).flatMap { mvar.put(b) }
          .bracket(use = { never<Int>() }, release = { just(Unit) })
        val (_, cancel) = ctx.startFiber(task).bind()
        p.get().bind()
        ctx.startFiber(cancel).bind()
        continueOn(ctx)
        mvar.take().bind()
        mvar.take().bind()
      }.a.eqv(just(b), EQ)
    }

  fun <F, A> Concurrent<F>.releaseBracketIsNotCancelable(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext) =
    forAll(Tuple2.arbitrary(arbA, arbA)) { (a, b) ->
      bindingCancellable {
        val mvar = MVar(a, this@releaseBracketIsNotCancelable).bind()
        val p = Promise.uncancelable<F, Unit>(this@releaseBracketIsNotCancelable).bind()
        val task = p.complete(Unit)
          .bracket(use = { never<Int>() }, release = { mvar.put(b) })
        val (_, cancel) = ctx.startFiber(task).bind()
        p.get().bind()
        ctx.startFiber(cancel).bind()
        continueOn(ctx)
        mvar.take().bind()
        mvar.take().bind()
      }.a.eqv(just(b), EQ)
    }

  fun <F, A> Concurrent<F>.asyncCancelableCoherence(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Either.arbitrary(NonFatalThrowableArbitrary, arbA)) { eith ->
      async<A> { cb -> cb(eith) }
        .eqv(cancelable { cb -> cb(eith); just<Unit>(Unit) }, EQ)
    }

  fun <F, A> Concurrent<F>.cancelableCancelableFCoherence(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Either.arbitrary(NonFatalThrowableArbitrary, arbA)) { eith ->
      cancelable<A> { cb -> cb(eith); just<Unit>(Unit) }
        .eqv(cancelableF { cb -> delay { cb(eith); just<Unit>(Unit) } }, EQ)
    }

  fun <F, A> Concurrent<F>.cancelableReceivesCancelSignal(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      bindingCancellable {
        val release = Promise.uncancelable<F, A>(this@cancelableReceivesCancelSignal).bind()
        val cancelToken: CancelToken<F> = release.complete(i)
        val latch = CountDownLatch(1)

        val (_, cancel) = ctx.startFiber(cancelable<Unit> {
          latch.countDown()
          cancelToken
        }).bind()

        ctx.shift().followedBy(asyncF<Unit> { cb ->
          delay { latch.await(500, TimeUnit.MILLISECONDS) }
            .map { cb(Right(Unit)) }
        }).bind()

        cancel.bind()
        release.get().bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.cancelableFReceivesCancelSignal(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      bindingCancellable {
        val release = Promise<F, A>(this@cancelableFReceivesCancelSignal).bind()
        val latch = Promise<F, Unit>(this@cancelableFReceivesCancelSignal).bind()
        val async = cancelableF<Unit> {
          latch.complete(Unit)
            .map { release.complete(i) }
        }
        val (_, cancel) = ctx.startFiber(async).bind()
        asyncF<Unit> { cb -> latch.get().map { cb(Right(it)) } }.bind()
        cancel.bind()
        release.get().bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.asyncCanCancelUpstream(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      bindingCancellable {
        val latch = Promise<F, A>(this@asyncCanCancelUpstream).bind()
        val cancelToken = AtomicReference<CancelToken<F>>()
        val cancelLatch = CountDownLatch(1)

        val upstream = async<Unit> { conn, cb ->
          conn.push(latch.complete(i))
          cb(Right(Unit))
        }

        val downstream = async<Unit> { conn, _ ->
          cancelToken.set(conn.cancel())
          cancelLatch.countDown()
        }

        ctx.startFiber(upstream.followedBy(downstream)).bind()

        ctx.startFiber(delay(ctx) {
          cancelLatch.await(500, TimeUnit.MILLISECONDS)
        }.flatMap { cancelToken.get() ?: raiseError(AssertionError("CancelToken was not set.")) }
        ).bind()

        latch.get().bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.asyncShouldRunKindConnectionOnCancel(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      bindingCancellable {
        val latch = Promise<F, A>(this@asyncShouldRunKindConnectionOnCancel).bind()
        val startLatch = CountDownLatch(1)

        val (_, cancel) = ctx.startFiber(async<Unit> { conn, _ ->
          conn.push(latch.complete(i))
          startLatch.countDown()
        }).bind()

        delay(ctx) {
          startLatch.await(500, TimeUnit.MILLISECONDS)
        }.followedBy(cancel).bind()

        latch.get().bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.asyncFRegisterCanBeCancelled(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      bindingCancellable {
        val release = Promise<F, A>(this@asyncFRegisterCanBeCancelled).bind()
        val acquire = Promise<F, Unit>(this@asyncFRegisterCanBeCancelled).bind()
        val task = asyncF<Unit> { _, _ ->
          acquire.complete(Unit).bracket(use = { never<Unit>() }, release = { release.complete(i) })
        }
        val (_, cancel) = ctx.startFiber(task).bind()
        acquire.get().bind()
        ctx.startFiber(cancel).bind()
        release.get().bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.asyncFCanCancelUpstream(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      bindingCancellable {
        val latch = Promise<F, A>(this@asyncFCanCancelUpstream).bind()
        val upstream = async<Unit> { conn, cb ->
          conn.push(latch.complete(i))
          cb(Right(Unit))
        }
        val downstream = asyncF<Unit> { conn, _ ->
          conn.cancel()
        }

        ctx.startFiber(upstream.followedBy(downstream)).bind()

        latch.get().bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.asyncFShouldRunKindConnectionOnCancel(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      bindingCancellable {
        val latch = Promise<F, A>(this@asyncFShouldRunKindConnectionOnCancel).bind()
        val startLatch = Promise<F, Unit>(this@asyncFShouldRunKindConnectionOnCancel).bind()

        val (_, cancel) = ctx.startFiber(asyncF<Unit> { conn, _ ->
          conn.push(latch.complete(i))
          //Wait with cancellation until it is run, if it doesn't run its cancellation is also doesn't run.
          startLatch.complete(Unit)
        }).bind()

        startLatch.get().flatMap { cancel }.bind()

        latch.get().bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.startJoinIsIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbF) { fa ->
      ctx.startFiber(fa).flatMap { it.join() }.eqv(fa, EQ)
    }

  fun <F, A> Concurrent<F>.joinIsIdempotent(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      Promise<F, A>(this@joinIsIdempotent).flatMap { p ->
        ctx.startFiber(p.complete(i))
          .flatMap { (join, _) -> join.followedBy(join) }
          .flatMap { p.get() }
      }.eqv(just(i), EQ)
    }

  fun <F> Concurrent<F>.startCancelIsUnit(EQ_UNIT: Eq<Kind<F, Unit>>, ctx: CoroutineContext): Property =
    forAll(applicativeErrorArbitrary(Int.arbitrary(), ThrowableArbitrary, this)) { fa ->
      ctx.startFiber(fa).flatMap { (_, cancel) -> cancel }
        .eqv(just<Unit>(Unit), EQ_UNIT)
    }

  fun <F, A> Concurrent<F>.uncancelableMirrorsSource(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbA) { i ->
      just(i).uncancelable().eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.raceMirrorsLeftWinner(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbF) { fa ->
      ctx.raceN(fa, never<A>()).flatMap { either ->
        either.fold({ just(it) }, { raiseError(IllegalStateException("never() finished race")) })
      }.eqv(fa, EQ)
    }

  fun <F, A> Concurrent<F>.raceMirrorsRightWinner(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbF) { fa ->
      ctx.raceN(never<A>(), fa).flatMap { either ->
        either.fold({ raiseError<A>(IllegalStateException("never() finished race")) }, { just(it) })
      }.eqv(fa, EQ)
    }

  fun <F, A> Concurrent<F>.raceCancelsLoser(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple3.arbitrary(
      Either.arbitrary(NonFatalThrowableArbitrary, arbA),
      Boolean.arbitrary(),
      arbA
    )) { (eith, leftWins, i) ->
      bindingCancellable {
        val s = Semaphore(0L, this@raceCancelsLoser).bind()
        val promise = Promise.uncancelable<F, A>(this@raceCancelsLoser).bind()
        val winner = s.acquire().flatMap { async<A> { cb -> cb(eith) } }
        val loser = s.release().bracket(use = { never<A>() }, release = { promise.complete(i) })
        val race =
          if (leftWins) ctx.raceN(winner, loser)
          else ctx.raceN(loser, winner)

        race.attempt().flatMap { promise.get() }.bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.raceCancelCancelsBoth(arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple2.arbitrary(arbA, arbA)) { (a, b) ->
      bindingCancellable {
        val s = Semaphore(0L, this@raceCancelCancelsBoth).bind()
        val pa = Promise<F, A>(this@raceCancelCancelsBoth).bind()
        val pb = Promise<F, A>(this@raceCancelCancelsBoth).bind()

        val loserA = s.release().bracket(use = { never<A>() }, release = { pa.complete(a) })
        val loserB = s.release().bracket(use = { never<A>() }, release = { pb.complete(b) })

        val (_, cancelRace) = ctx.startFiber(ctx.raceN(loserA, loserB)).bind()
        s.acquireN(2L).flatMap { cancelRace }.bind()
        MA.run { pa.get().bind() + pb.get().bind() }
      }.a.eqv(just(MA.run { a + b }), EQ)
    }

  fun <F, A> Concurrent<F>.raceCanBeCancelledByParticipants(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple2.arbitrary(arbA, Boolean.arbitrary())) { (i, shouldLeftCancel) ->
      bindingCancellable {
        val endLatch = Promise<F, A>(this@raceCanBeCancelledByParticipants).bind()
        val startLatch = Promise<F, Unit>(this@raceCanBeCancelledByParticipants).bind()

        val cancel = asyncF<Unit> { conn, cb -> startLatch.get().flatMap { conn.cancel().map { cb(Right(Unit)) } } }
        val loser = startLatch.complete(Unit) //guarantees that both cancel & loser started
          .bracket(use = { never<A>() }, release = { endLatch.complete(i) })

        if (shouldLeftCancel) ctx.startFiber(ctx.raceN(cancel, loser)).bind()
        else ctx.startFiber(ctx.raceN(loser, cancel)).bind()

        endLatch.get().bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.racePairMirrorsLeftWinner(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbF) { fa ->
      val never = never<A>()
      val received = ctx.racePair(fa, never).flatMap { either ->
        either.fold({ (a, fiberB) ->
          fiberB.cancel().map { a }
        }, { raiseError(AssertionError("never() finished race")) })
      }

      received.eqv(ctx.raceN(fa, never).map { it.fold(::identity, ::identity) }, EQ)
    }

  fun <F, A> Concurrent<F>.racePairMirrorsRightWinner(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbF) { fa ->
      val never = never<A>()
      val received = ctx.racePair(never, fa).flatMap { either ->
        either.fold({
          raiseError<A>(AssertionError("never() finished race"))
        }, { (fiberA, b) -> fiberA.cancel().map { b } })
      }

      received.eqv(ctx.raceN(never, fa).map { it.fold(::identity, ::identity) }, EQ)
    }

  fun <F, A> Concurrent<F>.racePairCanCancelsLoser(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple3.arbitrary(
      Either.arbitrary(NonFatalThrowableArbitrary, arbA),
      Boolean.arbitrary(),
      arbA
    )) { (eith, leftWinner, i) ->
      val received = bindingCancellable {
        val s = Semaphore(0L, this@racePairCanCancelsLoser).bind()
        val p = Promise.uncancelable<F, A>(this@racePairCanCancelsLoser).bind()
        val winner = s.acquire().flatMap { async<A> { cb -> cb(eith) } }
        val loser = s.release().bracket(use = { never<String>() }, release = { p.complete(i) })
        val race = if (leftWinner) ctx.racePair(winner, loser)
        else ctx.racePair(loser, winner)

        race.attempt()
          .flatMap { attempt ->
            attempt.fold({ p.get() },
              {
                it.fold(
                  { (_, fiberB) -> ctx.startFiber(fiberB.cancel()).flatMap { p.get() } },
                  { (fiberA, _) -> ctx.startFiber(fiberA.cancel()).flatMap { p.get() } })
              })
          }.bind()
      }

      received.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.racePairCanJoinLeft(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      Promise<F, A>(this@racePairCanJoinLeft).flatMap { p ->
        ctx.racePair(p.get(), just(Unit)).flatMap { eith ->
          eith.fold(
            { (unit, _) -> just(unit) },
            { (fiber, _) -> p.complete(i).flatMap { fiber.join() } }
          )
        }
      }.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.racePairCanJoinRight(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      Promise<F, A>(this@racePairCanJoinRight).flatMap { p ->
        ctx.racePair(just(Unit), p.get()).flatMap { eith ->
          eith.fold(
            { (_, fiber) -> p.complete(i).flatMap { fiber.join() } },
            { (_, unit) -> just(unit) }
          )
        }
      }.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.racePairCancelCancelsBoth(arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple2.arbitrary(arbA, arbA)) { (a, b) ->
      bindingCancellable {
        val s = Semaphore(0L, this@racePairCancelCancelsBoth).bind()
        val pa = Promise<F, A>(this@racePairCancelCancelsBoth).bind()
        val pb = Promise<F, A>(this@racePairCancelCancelsBoth).bind()

        val loserA: Kind<F, A> = s.release().bracket(use = { never<A>() }, release = { pa.complete(a) })
        val loserB: Kind<F, A> = s.release().bracket(use = { never<A>() }, release = { pb.complete(b) })

        val (_, cancelRacePair) = ctx.startFiber(ctx.racePair(loserA, loserB)).bind()

        s.acquireN(2L).flatMap { cancelRacePair }.bind()
        MA.run { pa.get().bind() + pb.get().bind() }
      }.a.eqv(just(MA.run { a + b }), EQ)
    }

  fun <F, A> Concurrent<F>.racePairCanBeCancelledByParticipants(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple2.arbitrary(arbA, Boolean.arbitrary())) { (i, shouldLeftCancel) ->
      bindingCancellable {
        val endLatch = Promise<F, A>(this@racePairCanBeCancelledByParticipants).bind()
        val startLatch = Promise<F, Unit>(this@racePairCanBeCancelledByParticipants).bind()

        val cancel = asyncF<Unit> { conn, cb -> startLatch.get().flatMap { conn.cancel().map { cb(Right(Unit)) } } }

        val loser = startLatch.complete(Unit) //guarantees that both cancel & loser actually started
          .bracket(use = { never<A>() }, release = { endLatch.complete(i) })

        if (shouldLeftCancel) ctx.startFiber(ctx.racePair(cancel, loser)).bind()
        else ctx.startFiber(ctx.racePair(loser, cancel)).bind()

        endLatch.get().bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.raceTripleMirrorsLeftWinner(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbF) { fa ->
      val never = never<A>()
      val received = ctx.raceTriple(fa, never, never).flatMap { either ->
        either.fold(
          { (a, fiberB, fiberC) -> fiberB.cancel().followedBy(fiberC.cancel()).map { a } },
          { raiseError(AssertionError("never() finished race")) },
          { raiseError(AssertionError("never() finished race")) })
      }

      received.eqv(ctx.raceN(fa, never, never).map { it.fold(::identity, ::identity, ::identity) }, EQ)
    }

  fun <F, A> Concurrent<F>.raceTripleMirrorsMiddleWinner(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbF) { fa ->
      val never = never<A>()
      val received = ctx.raceTriple(never, fa, never).flatMap { either ->
        either.fold(
          { raiseError<A>(AssertionError("never() finished race")) },
          { (fiberA, b, fiberC) -> fiberA.cancel().followedBy(fiberC.cancel()).map { b } },
          { raiseError(AssertionError("never() finished race")) })
      }

      received.eqv(ctx.raceN(never, fa, never).map { it.fold(::identity, ::identity, ::identity) }, EQ)
    }

  fun <F, A> Concurrent<F>.raceTripleMirrorsRightWinner(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbF) { fa ->
      val never = never<A>()
      val received = ctx.raceTriple(never, never, fa).flatMap { either ->
        either.fold(
          { raiseError<A>(AssertionError("never() finished race")) },
          { raiseError(AssertionError("never() finished race")) },
          { (fiberA, fiberB, c) -> fiberA.cancel().followedBy(fiberB.cancel()).map { c } })
      }

      received.eqv(ctx.raceN(never, never, fa).map { it.fold(::identity, ::identity, ::identity) }, EQ)
    }

  fun <F, A> Concurrent<F>.raceTripleCanCancelsLoser(arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple4.arbitrary(
      Either.arbitrary(ThrowableArbitrary, arbA),
      Arbitrary(Gen.elements(1, 2, 3)),
      arbA,
      arbA
    )) { (eith, leftWinner, a, b) ->
      val received = bindingCancellable {
        val s = Semaphore(0L, this@raceTripleCanCancelsLoser).bind()
        val pa = Promise.uncancelable<F, A>(this@raceTripleCanCancelsLoser).bind()
        val pb = Promise.uncancelable<F, A>(this@raceTripleCanCancelsLoser).bind()

        val winner = s.acquireN(2).flatMap { async<A> { cb -> cb(eith) } }
        val loser = s.release().bracket(use = { never<A>() }, release = { pa.complete(a) })
        val loser2 = s.release().bracket(use = { never<A>() }, release = { pb.complete(b) })

        val race = when (leftWinner) {
          1 -> ctx.raceTriple(winner, loser, loser2)
          2 -> ctx.raceTriple(loser, winner, loser2)
          else -> ctx.raceTriple(loser, loser2, winner)
        }

        val combinePromises = pa.get().flatMap { a -> pb.get().map { b -> MA.run { a + b } } }

        race.attempt()
          .flatMap { attempt ->
            attempt.fold({ combinePromises },
              {
                it.fold(
                  { (_, fiberB, fiberC) ->
                    ctx.startFiber(fiberB.cancel().followedBy(fiberC.cancel())).flatMap { combinePromises }
                  },
                  { (fiberA, _, fiberC) ->
                    ctx.startFiber(fiberA.cancel().followedBy(fiberC.cancel())).flatMap { combinePromises }
                  },
                  { (fiberA, fiberB, _) ->
                    ctx.startFiber(fiberA.cancel().followedBy(fiberB.cancel())).flatMap { combinePromises }
                  })
              })
          }.bind()
      }

      received.a.eqv(just(MA.run { a + b }), EQ)
    }

  fun <F, A> Concurrent<F>.raceTripleCanJoinLeft(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      Promise<F, A>(this@raceTripleCanJoinLeft).flatMap { p ->
        ctx.raceTriple(p.get(), just(Unit), never<Unit>()).flatMap { result ->
          result.fold(
            { raiseError<A>(AssertionError("Promise#get can never win race")) },
            { (fiber, _, _) -> p.complete(i).flatMap { fiber.join() } },
            { raiseError(AssertionError("never() can never win race")) }
          )
        }
      }.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.raceTripleCanJoinMiddle(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      Promise<F, A>(this@raceTripleCanJoinMiddle).flatMap { p ->
        ctx.raceTriple(just(Unit), p.get(), never<Unit>()).flatMap { result ->
          result.fold(
            { (_, fiber, _) -> p.complete(i).flatMap { fiber.join() } },
            { raiseError(AssertionError("Promise#get can never win race")) },
            { raiseError(AssertionError("never() can never win race")) }
          )
        }
      }.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.raceTripleCanJoinRight(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(arbA) { i ->
      Promise<F, A>(this@raceTripleCanJoinRight).flatMap { p ->
        ctx.raceTriple(just(Unit), never<Unit>(), p.get()).flatMap { result ->
          result.fold(
            { (_, _, fiber) -> p.complete(i).flatMap { fiber.join() } },
            { raiseError(AssertionError("never() can never win race")) },
            { raiseError(AssertionError("Promise#get can never win race")) }
          )
        }
      }.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.raceTripleCanBeCancelledByParticipants(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext) =
    forAll(Tuple2.arbitrary(
      arbA,
      Arbitrary(Gen.elements(1, 2, 3))
    )) { (i, shouldCancel) ->
      bindingCancellable {
        val endLatch = Promise<F, A>(this@raceTripleCanBeCancelledByParticipants).bind()
        val startLatch = Promise<F, Unit>(this@raceTripleCanBeCancelledByParticipants).bind()
        val start2Latch = Promise<F, Unit>(this@raceTripleCanBeCancelledByParticipants).bind()

        val cancel = asyncF<Unit> { conn, cb ->
          startLatch.get().followedBy(start2Latch.get())
            .flatMap { conn.cancel().map { cb(Right(Unit)) } }
        }

        val loser = startLatch.complete(Unit) //guarantees that both cancel & loser actually started
          .bracket(use = { never<A>() }, release = { endLatch.complete(i) })
        val loser2 = start2Latch.complete(Unit) //guarantees that both cancel & loser actually started
          .bracket(use = { never<A>() }, release = { endLatch.complete(i) })

        when (shouldCancel) {
          1 -> ctx.startFiber(ctx.raceTriple(cancel, loser, loser2)).bind()
          2 -> ctx.startFiber(ctx.raceTriple(loser, cancel, loser2)).bind()
          else -> ctx.startFiber(ctx.raceTriple(loser, loser2, cancel)).bind()
        }

        endLatch.get().bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.raceTripleCancelCancelsAll(arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple3.arbitrary(arbA, arbA, arbA)) { (a, b, c) ->
      bindingCancellable {
        val s = Semaphore(0L, this@raceTripleCancelCancelsAll).bind()
        val pa = Promise<F, A>(this@raceTripleCancelCancelsAll).bind()
        val pb = Promise<F, A>(this@raceTripleCancelCancelsAll).bind()
        val pc = Promise<F, A>(this@raceTripleCancelCancelsAll).bind()

        val loserA: Kind<F, A> = s.release().bracket(use = { never<A>() }, release = { pa.complete(a) })
        val loserB: Kind<F, A> = s.release().bracket(use = { never<A>() }, release = { pb.complete(b) })
        val loserC: Kind<F, A> = s.release().bracket(use = { never<A>() }, release = { pc.complete(c) })

        val (_, cancelRacePair) = ctx.startFiber(ctx.raceTriple(loserA, loserB, loserC)).bind()

        s.acquireN(3L).flatMap { cancelRacePair }.bind()
        MA.run { pa.get().bind() + pb.get().bind() + pc.get().bind() }
      }.a.eqv(just(MA.run { a + b + c }), EQ)
    }

  fun <F, A> Concurrent<F>.parMapCancelCancelsBoth(arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple2.arbitrary(arbA, arbA)) { (a, b) ->
      bindingCancellable {
        val s = Semaphore(0L, this@parMapCancelCancelsBoth).bind()
        val pa = Promise<F, A>(this@parMapCancelCancelsBoth).bind()
        val pb = Promise<F, A>(this@parMapCancelCancelsBoth).bind()

        val loserA = s.release().bracket(use = { never<A>() }, release = { pa.complete(a) })
        val loserB = s.release().bracket(use = { never<A>() }, release = { pb.complete(b) })

        val (_, cancelParMapN) = ctx.startFiber(ctx.parMapN(loserA, loserB, ::Tuple2)).bind()
        s.acquireN(2L).flatMap { cancelParMapN }.bind()
        MA.run { pa.get().bind() + pb.get().bind() }
      }.a.eqv(just(MA.run { a + b }), EQ)
    }

  fun <F, A> Concurrent<F>.parMapCanBeCancelledByParticipants(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple2.arbitrary(arbA, Boolean.arbitrary())) { (i, shouldLeftCancel) ->
      bindingCancellable {
        val endLatch = Promise<F, A>(this@parMapCanBeCancelledByParticipants).bind()
        val startLatch = Promise<F, Unit>(this@parMapCanBeCancelledByParticipants).bind()

        val cancel = asyncF<Unit> { conn, cb -> startLatch.get().flatMap { conn.cancel().map { cb(Right(Unit)) } } }
        val loser = startLatch.complete(Unit).bracket(use = { never<A>() }, release = { endLatch.complete(i) })

        if (shouldLeftCancel) ctx.startFiber(ctx.parMapN(cancel, loser, ::Tuple2)).bind()
        else ctx.startFiber(ctx.parMapN(loser, cancel, ::Tuple2)).bind()

        endLatch.get().bind()
      }.a.eqv(just(i), EQ)
    }

  fun <F, A> Concurrent<F>.actionConcurrentWithPureValueIsJustAction(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>, ctx: CoroutineContext): Property =
    forAll(Tuple2.arbitrary(
      Arbitrary(arbA.arbitrary().map(::just)),
      arbA
    )) { (fa, i) ->
      ctx.startFiber(i.just()).flatMap { (join, _) ->
        fa.flatMap {
          join.map { i }
        }
      }.eqv(just(i), EQ)
    }
}
