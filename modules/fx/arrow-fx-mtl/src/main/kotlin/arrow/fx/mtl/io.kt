package arrow.fx.mtl

import arrow.Kind
import arrow.extension
import arrow.fx.ForIO
import arrow.fx.IO
import arrow.fx.extensions.io.monad.monad
import arrow.fx.fix
import arrow.mtl.typeclasses.MonadBase
import arrow.mtl.typeclasses.MonadBaseControl
import arrow.mtl.typeclasses.RunInBase
import arrow.mtl.typeclasses.StM
import arrow.typeclasses.Monad

@extension
interface IOMonadBase : MonadBase<ForIO, ForIO> {
  override fun MB(): Monad<ForIO> = IO.monad()
  override fun MM(): Monad<ForIO> = IO.monad()
  override fun <A> Kind<ForIO, A>.liftBase(): Kind<ForIO, A> = this
}

@extension
interface IOMonadBaseControl : MonadBaseControl<ForIO, ForIO>, IOMonadBase {
  override fun <A> liftBaseWith(runInBase: (RunInBase<ForIO, ForIO>) -> Kind<ForIO, A>): Kind<ForIO, A> =
    runInBase(object : RunInBase<ForIO, ForIO> {
      override fun <A> invoke(ma: Kind<ForIO, A>): Kind<ForIO, StM<ForIO, A>> =
        ma.fix().map { StM<ForIO, A>(it) }
    })

  override fun <A> StM<ForIO, A>.restoreM(): Kind<ForIO, A> = IO.just(st as A)

  override fun <A> Kind<ForIO, A>.liftBase(): Kind<ForIO, A> = this
}
