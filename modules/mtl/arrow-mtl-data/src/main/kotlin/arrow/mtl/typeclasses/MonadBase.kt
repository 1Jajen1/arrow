package arrow.mtl.typeclasses

import arrow.Kind
import arrow.typeclasses.Monad

interface MonadBase<B, M> {
  fun MM(): Monad<M>
  fun MB(): Monad<B>

  fun <A> Kind<B, A>.liftBase(): Kind<M, A>
}

fun <F> monadBaseId(MF: Monad<F>): MonadBase<F, F> = object : MonadBase<F, F> {
  override fun MB(): Monad<F> = MF
  override fun MM(): Monad<F> = MF
  override fun <A> Kind<F, A>.liftBase(): Kind<F, A> = this
}

fun <T, M, B> monadBaseDefault(MT: MonadTrans<T>, MB: MonadBase<B, M>): MonadBase<B, Kind<T, M>> = object : MonadBase<B, Kind<T, M>> {
  override fun MB(): Monad<B> = MB.MB()
  override fun MM(): Monad<Kind<T, M>> = MT.monad(MB.MM())

  override fun <A> Kind<B, A>.liftBase(): Kind<Kind<T, M>, A> = MB.run { MT.run { liftBase().liftT(MB.MM()) } }
}
