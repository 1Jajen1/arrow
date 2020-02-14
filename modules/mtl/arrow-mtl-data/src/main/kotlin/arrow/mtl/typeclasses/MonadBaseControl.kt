package arrow.mtl.typeclasses

import arrow.Kind
import arrow.typeclasses.Monad

interface MonadBaseControl<B, M> : MonadBase<B, M> {

  fun <A> liftBaseWith(runInBase: (RunInBase<M, B>) -> Kind<B, A>): Kind<M, A>

  override fun <A> Kind<B, A>.liftBase(): Kind<M, A> = liftBaseWith { this }

  fun <A> StM<M, A>.restoreM(): Kind<M, A>

  fun <A> control(f: (RunInBase<M, B>) -> Kind<B, StM<M, A>>): Kind<M, A> = MM().run {
    liftBaseWith(f).flatMap { it.restoreM() }
  }
}

interface RunInBase<M, B> {
  operator fun <A> invoke(ma: Kind<M, A>): Kind<B, StM<M, A>>
}

data class StM<M, A>(val st: Any?)

fun <F> monadBaseControlId(MF: Monad<F>): MonadBaseControl<F, F> = object : MonadBaseControl<F, F> {
  override fun MB(): Monad<F> = MF
  override fun MM(): Monad<F> = MF
  override fun <A> liftBaseWith(runInBase: (RunInBase<F, F>) -> Kind<F, A>): Kind<F, A> =
    runInBase(object : RunInBase<F, F> {
      override fun <A> invoke(ma: Kind<F, A>): Kind<F, StM<F, A>> = MF.run { ma.map { StM<F, A>(it) } }
    })

  override fun <A> StM<F, A>.restoreM(): Kind<F, A> = MF.just(st as A)
}

fun <T, M, B> defaultMonadBaseControl(
  MB: Monad<B>,
  MM: Monad<M>,
  MT: MonadTransControl<T>,
  MBB: MonadBaseControl<B, M>
): MonadBaseControl<B, Kind<T, M>> = object : MonadBaseControl<B, Kind<T, M>> {
  override fun MB(): Monad<B> = MB
  override fun MM(): Monad<Kind<T, M>> = MT.monad(MM)

  override fun <A> liftBaseWith(runInBase: (RunInBase<Kind<T, M>, B>) -> Kind<B, A>): Kind<Kind<T, M>, A> =
    MT.liftWith(MM) { run ->
      MBB.liftBaseWith { runInM ->
        runInBase(object : RunInBase<Kind<T, M>, B> {
          override fun <A> invoke(ma: Kind<Kind<T, M>, A>): Kind<B, StM<Kind<T, M>, A>> =
            MB.run { runInM(run(MM, ma)).map { StM<Kind<T, M>, A>(it) } }
        })
      }
    }

  override fun <A> StM<Kind<T, M>, A>.restoreM(): Kind<Kind<T, M>, A> =
    MBB.run {
      MT.run {
        (st as StM<M, StT<T, A>>).restoreM().restoreT(MM)
      }
    }
}
