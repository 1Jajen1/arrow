package arrow.fx.mtl

import arrow.Kind
import arrow.core.None
import arrow.core.Option
import arrow.core.internal.AtomicRefW
import arrow.core.none
import arrow.core.some
import arrow.core.toT
import arrow.fx.RacePair
import arrow.fx.RaceTriple
import arrow.fx.typeclasses.Bracket
import arrow.fx.typeclasses.Concurrent
import arrow.fx.typeclasses.ExitCase
import arrow.fx.typeclasses.Fiber
import arrow.fx.typeclasses.MonadDefer
import arrow.mtl.typeclasses.MonadBaseControl
import arrow.mtl.typeclasses.StM
import arrow.typeclasses.MonadError
import kotlin.coroutines.CoroutineContext

fun <F, A, C, B, E> Kind<F, A>.defaultBracket(
  BR: Bracket<B, E>,
  MBC: MonadBaseControl<B, F>,
  release: (A, ExitCase<E>) -> Kind<F, Unit>,
  use: (A) -> Kind<F, C>
): Kind<F, C> = MBC.liftBaseWith { runInBase ->
  BR.run {
    val atomic: AtomicRefW<Option<StM<F, Any?>>> = AtomicRefW(None)
      runInBase(this@defaultBracket).bracketCase({ stmA, exitCase ->
        MBC.run {
          MBC.MM().run {
            when (exitCase) {
              is ExitCase.Completed ->
                // Apply monad state of use, which is guaranteed to exist because use completed
              runInBase(stmA.restoreM().flatMap { a -> atomic.value.orNull()!!.restoreM().flatMap { release(a, exitCase) } })
                .map { atomic.value = (it as StM<F, Any?>).some() }
              else ->
                runInBase(stmA.restoreM().flatMap { a -> release(a, exitCase) }).unit()
            }
          }
        }
      }, { stmA ->
        MBC.run { MBC.MM().run { runInBase(stmA.restoreM().flatMap(use)).map { it.also { atomic.value = (it as StM<F, Any?>).some() } } } }
      }).map { st ->
        atomic.value.orNull()!! toT st
      }
    }
}.let {
  MBC.MM().run {
    it.flatMap { (releaseSt, useSt) ->
      MBC.run { useSt.restoreM().flatTap { releaseSt.restoreM() } }
    }
  }
}

fun <F, G, A, B, E> CoroutineContext.defaultRacePair(
  CB: Concurrent<G>,
  ME: MonadError<F, E>,
  MBC: MonadBaseControl<G, F>,
  fa: Kind<F, A>,
  fb: Kind<F, B>
): Kind<F, RacePair<F, A, B>> = CB.run {
  MBC.run {
    liftBaseWith { runInBase ->
      racePair(runInBase(fa), runInBase(fb)).map { res ->
        when (res) {
          is RacePair.First -> ME.run {
            res.winner.restoreM().map { a ->
              RacePair.First(a, Fiber(control { res.fiberB.join() }, res.fiberB.cancel().liftBase()))
            }.handleErrorWith { e -> res.fiberB.cancel().liftBase().flatMap { raiseError<RacePair<F, A, B>>(e) } }
          }
          is RacePair.Second -> ME.run {
            res.winner.restoreM().map { b ->
              RacePair.Second(Fiber(control { res.fiberA.join() }, res.fiberA.cancel().liftBase()), b)
            }.handleErrorWith { e -> res.fiberA.cancel().liftBase().flatMap { raiseError<RacePair<F, A, B>>(e) } }
          }
        }
      }
    }.let { MBC.MM().run { it.flatten() } }
  }
}

fun <F, G, A, B, C, E> CoroutineContext.defaultRaceTriple(
  CB: Concurrent<G>,
  ME: MonadError<F, E>,
  MBC: MonadBaseControl<G, F>,
  fa: Kind<F, A>,
  fb: Kind<F, B>,
  fc: Kind<F, C>
): Kind<F, RaceTriple<F, A, B, C>> = CB.run {
  MBC.run {
    ME.run {
      liftBaseWith { runInBase -> raceTriple(runInBase(fa), runInBase(fb), runInBase(fc)) }.flatMap { res ->
        when (res) {
          is RaceTriple.First ->
            res.winner.restoreM().map { a ->
              RaceTriple.First(
                a,
                Fiber(control { res.fiberB.join() }, res.fiberB.cancel().liftBase()),
                Fiber(control { res.fiberC.join() }, res.fiberC.cancel().liftBase())
              )
            }.handleErrorWith { e -> tupledN(res.fiberB.cancel().liftBase(), res.fiberC.cancel().liftBase()).unit().flatMap { raiseError<RaceTriple<F, A, B, C>>(e) } }
          is RaceTriple.Second ->
            res.winner.restoreM().map { b ->
              RaceTriple.Second(
                Fiber(control { res.fiberA.join() }, res.fiberA.cancel().liftBase()),
                b,
                Fiber(control { res.fiberC.join() }, res.fiberC.cancel().liftBase())
              )
            }.handleErrorWith { e -> tupledN(res.fiberA.cancel().liftBase(), res.fiberC.cancel().liftBase()).unit().flatMap { raiseError<RaceTriple<F, A, B, C>>(e) } }
          is RaceTriple.Third ->
            res.winner.restoreM().map { c ->
              RaceTriple.Third(
                Fiber(control { res.fiberA.join() }, res.fiberA.cancel().liftBase()),
                Fiber(control { res.fiberB.join() }, res.fiberB.cancel().liftBase()),
                c
              )
            }.handleErrorWith { e -> tupledN(res.fiberA.cancel().liftBase(), res.fiberB.cancel().liftBase()).unit().flatMap { raiseError<RaceTriple<F, A, B, C>>(e) } }
        }
      }
    }
  }
}
