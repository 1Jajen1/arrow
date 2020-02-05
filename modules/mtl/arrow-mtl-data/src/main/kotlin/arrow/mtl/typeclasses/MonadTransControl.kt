package arrow.mtl.typeclasses

import arrow.Kind
import arrow.Kind2
import arrow.core.identity
import arrow.typeclasses.Monad

interface MonadTransControl<T> : MonadTrans<T> {
  fun <M, A> liftWith(MM: Monad<M>, runInM: (Run<T>) -> Kind<M, A>): Kind2<T, M, A>
  fun <M, A> Kind<M, StT<T, A>>.restoreT(MM: Monad<M>): Kind2<T, M, A>

  override fun <G, A> Kind<G, A>.liftT(MG: Monad<G>): Kind2<T, G, A> = liftWith(MG) { this }
}

interface Run<T> {
  operator fun <N, A> invoke(NM: Monad<N>, na: Kind2<T, N, A>): Kind<N, StT<T, A>>
}

/**
 * Marker for a transformers state so that we can cast the content to whatever the interface needs safely. It holds T and A as phantom params to
 *  ensure it is called with the correct state.
 */
data class StT<T, A>(val st: Any?)
