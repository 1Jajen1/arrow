package arrow.fx.mtl

import arrow.Kind
import arrow.core.none
import arrow.core.some
import arrow.core.toT
import arrow.fx.RacePair
import arrow.fx.RaceTriple
import arrow.fx.typeclasses.Concurrent
import arrow.fx.typeclasses.ExitCase
import arrow.fx.typeclasses.Fiber
import arrow.fx.typeclasses.MonadDefer
import arrow.mtl.typeclasses.MonadBaseControl
import arrow.mtl.typeclasses.StM
import arrow.typeclasses.MonadError
import kotlin.coroutines.CoroutineContext

fun <F, A, C, B> Kind<F, A>.defaultBracket(
  MD: MonadDefer<B>,
  MBC: MonadBaseControl<B, F>,
  release: (A, ExitCase<Throwable>) -> Kind<F, Unit>,
  use: (A) -> Kind<F, C>
): Kind<F, C> = MBC.liftBaseWith { runInBase ->
  MD.run {
    Ref(none<StM<F, Any?>>()).flatMap { ref ->
      runInBase(this@defaultBracket).bracketCase({ stmA, exitCase ->
        MBC.run {
          MBC.MM().run {
            when (exitCase) {
              is ExitCase.Completed ->
                // Apply monad state of use, which is guaranteed to exist because use completed
                ref.get().flatMap { runInBase(stmA.restoreM().flatMap { a -> it.orNull()!!.restoreM().flatMap { release(a, exitCase) } }) }
                  .flatMap { ref.set((it as StM<F, Any?>).some()) }
              else ->
                runInBase(stmA.restoreM().flatMap { a -> release(a, exitCase) }).unit()
            }
          }
        }
      }, { stmA ->
        MBC.run { MBC.MM().run { runInBase(stmA.restoreM().flatMap(use)).flatTap { ref.set((it as StM<F, Any?>).some()) } } }
      }).flatMap { st ->
        ref.get().map { it.orNull()!! toT st }
      }
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
              RacePair.First(a, Fiber(MBC.control { res.fiberB.join() }, MBC.run { res.fiberB.cancel().liftBase() }))
            }.handleErrorWith { e -> res.fiberB.cancel().liftBase().flatMap { raiseError<RacePair<F, A, B>>(e) } }
          }
          is RacePair.Second -> ME.run {
            res.winner.restoreM().map { b ->
              RacePair.Second(Fiber(MBC.control { res.fiberA.join() }, MBC.run { res.fiberA.cancel().liftBase() }), b)
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
                Fiber(MBC.control { res.fiberB.join() }, MBC.run { res.fiberB.cancel().liftBase() }),
                Fiber(MBC.control { res.fiberC.join() }, MBC.run { res.fiberC.cancel().liftBase() })
              )
            }.handleErrorWith { e -> tupledN(res.fiberB.cancel().liftBase(), res.fiberC.cancel().liftBase()).unit().flatMap { raiseError<RaceTriple<F, A, B, C>>(e) } }
          is RaceTriple.Second ->
            res.winner.restoreM().map { b ->
              RaceTriple.Second(
                Fiber(MBC.control { res.fiberA.join() }, MBC.run { res.fiberA.cancel().liftBase() }),
                b,
                Fiber(MBC.control { res.fiberC.join() }, MBC.run { res.fiberC.cancel().liftBase() })
              )
            }.handleErrorWith { e -> tupledN(res.fiberA.cancel().liftBase(), res.fiberC.cancel().liftBase()).unit().flatMap { raiseError<RaceTriple<F, A, B, C>>(e) } }
          is RaceTriple.Third ->
            res.winner.restoreM().map { c ->
              RaceTriple.Third(
                Fiber(MBC.control { res.fiberA.join() }, MBC.run { res.fiberA.cancel().liftBase() }),
                Fiber(MBC.control { res.fiberB.join() }, MBC.run { res.fiberB.cancel().liftBase() }),
                c
              )
            }.handleErrorWith { e -> tupledN(res.fiberA.cancel().liftBase(), res.fiberB.cancel().liftBase()).unit().flatMap { raiseError<RaceTriple<F, A, B, C>>(e) } }
        }
      }
    }
  }
}
