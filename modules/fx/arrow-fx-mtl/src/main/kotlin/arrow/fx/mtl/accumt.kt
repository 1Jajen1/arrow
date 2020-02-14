package arrow.fx.mtl

import arrow.Kind
import arrow.core.toT
import arrow.extension
import arrow.fx.IO
import arrow.fx.RacePair
import arrow.fx.RaceTriple
import arrow.fx.typeclasses.Async
import arrow.fx.typeclasses.Bracket
import arrow.fx.typeclasses.Concurrent
import arrow.fx.typeclasses.Dispatchers
import arrow.fx.typeclasses.ExitCase
import arrow.fx.typeclasses.Fiber
import arrow.fx.typeclasses.MonadDefer
import arrow.fx.typeclasses.MonadIO
import arrow.fx.typeclasses.ProcF
import arrow.mtl.AccumT
import arrow.mtl.AccumTPartialOf
import arrow.mtl.extensions.AccumTMonad
import arrow.mtl.extensions.AccumTMonadError
import arrow.mtl.extensions.accumt.monadError.monadError
import arrow.mtl.extensions.accumt.monadTrans.liftT
import arrow.mtl.extensions.monadBaseControl
import arrow.mtl.fix
import arrow.mtl.typeclasses.monadBaseControlId
import arrow.typeclasses.Monad
import arrow.typeclasses.MonadError
import arrow.typeclasses.Monoid
import kotlin.coroutines.CoroutineContext

@extension
interface AccumTMonadIO<S, F> : MonadIO<AccumTPartialOf<S, F>>, AccumTMonad<S, F> {
  fun FIO(): MonadIO<F>

  override fun MS(): Monoid<S>
  override fun MF(): Monad<F> = FIO()

  override fun <A> IO<A>.liftIO(): Kind<AccumTPartialOf<S, F>, A> = FIO().run {
    liftIO().liftT(MS(), MF())
  }
}

@extension
interface AccumTBracket<S, F, E> : Bracket<AccumTPartialOf<S, F>, E>, AccumTMonadError<S, F, E> {
  fun BR(): Bracket<F, E>
  override fun MS(): Monoid<S>
  override fun ME(): MonadError<F, E> = BR()

  override fun <A, B> Kind<AccumTPartialOf<S, F>, A>.bracketCase(release: (A, ExitCase<E>) -> Kind<AccumTPartialOf<S, F>, Unit>, use: (A) -> Kind<AccumTPartialOf<S, F>, B>): Kind<AccumTPartialOf<S, F>, B> =
    defaultBracket(BR(), AccumT.monadBaseControl(monadBaseControlId(BR()), MS()), release, use)
}

@extension
interface AccumTMonadDefer<S, F> : MonadDefer<AccumTPartialOf<S, F>>, AccumTBracket<S, F, Throwable> {
  fun MD(): MonadDefer<F>
  override fun BR(): Bracket<F, Throwable> = MD()
  override fun MS(): Monoid<S>
  override fun <A> defer(fa: () -> Kind<AccumTPartialOf<S, F>, A>): Kind<AccumTPartialOf<S, F>, A> =
    AccumT { MD().defer { fa().fix().runAccumT(it) } }
}

@extension
interface AccumTAsync<S, F> : Async<AccumTPartialOf<S, F>>, AccumTMonadDefer<S, F> {
  fun AS(): Async<F>
  override fun MD(): MonadDefer<F> = AS()
  override fun MS(): Monoid<S>

  override fun <A> Kind<AccumTPartialOf<S, F>, A>.continueOn(ctx: CoroutineContext): Kind<AccumTPartialOf<S, F>, A> =
    fix().mapAccumT { AS().run { it.continueOn(ctx) } }

  override fun <A> asyncF(k: ProcF<AccumTPartialOf<S, F>, A>): Kind<AccumTPartialOf<S, F>, A> =
    AccumT { s -> AS().run { asyncF<A> { cb -> k(cb).fix().runAccumT(s).unit() }.map { MS().empty() toT it } } }
}

@extension
interface AccumTConcurrent<S, F> : Concurrent<AccumTPartialOf<S, F>>, AccumTAsync<S, F> {
  fun CF(): Concurrent<F>
  override fun AS(): Async<F> = CF()
  override fun MS(): Monoid<S>

  override fun dispatchers(): Dispatchers<AccumTPartialOf<S, F>> = CF().dispatchers() as Dispatchers<AccumTPartialOf<S, F>>

  override fun <A> Kind<AccumTPartialOf<S, F>, A>.fork(ctx: CoroutineContext): Kind<AccumTPartialOf<S, F>, Fiber<AccumTPartialOf<S, F>, A>> =
    fix().mapAccumT { fa ->
      CF().run { fa.fork(ctx).map { MS().empty() toT Fiber(AccumT { _: S -> it.join() }, AccumT.liftF(MS(), CF(), it.cancel())) } }
    }

  override fun <A, B> CoroutineContext.racePair(fa: Kind<AccumTPartialOf<S, F>, A>, fb: Kind<AccumTPartialOf<S, F>, B>): Kind<AccumTPartialOf<S, F>, RacePair<AccumTPartialOf<S, F>, A, B>> =
    defaultRacePair(CF(), AccumT.monadError(MS(), CF()), AccumT.monadBaseControl(monadBaseControlId(CF()), MS()), fa, fb)

  override fun <A, B, C> CoroutineContext.raceTriple(fa: Kind<AccumTPartialOf<S, F>, A>, fb: Kind<AccumTPartialOf<S, F>, B>, fc: Kind<AccumTPartialOf<S, F>, C>): Kind<AccumTPartialOf<S, F>, RaceTriple<AccumTPartialOf<S, F>, A, B, C>> =
    defaultRaceTriple(CF(), AccumT.monadError(MS(), CF()), AccumT.monadBaseControl(monadBaseControlId(CF()), MS()), fa, fb, fc)
}
