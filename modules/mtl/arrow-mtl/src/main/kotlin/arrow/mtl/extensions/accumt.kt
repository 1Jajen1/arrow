package arrow.mtl.extensions

import arrow.Kind
import arrow.Kind2
import arrow.core.AndThen
import arrow.core.Either
import arrow.core.Tuple2
import arrow.core.toT
import arrow.extension
import arrow.mtl.AccumT
import arrow.mtl.AccumTPartialOf
import arrow.mtl.ForAccumT
import arrow.mtl.OptionT
import arrow.mtl.extensions.accumt.monad.monad
import arrow.mtl.extensions.accumt.monadTransControl.monadTransControl
import arrow.mtl.extensions.optiont.monadTransControl.monadTransControl
import arrow.mtl.fix
import arrow.mtl.typeclasses.MonadBase
import arrow.mtl.typeclasses.MonadBaseControl
import arrow.mtl.typeclasses.MonadReader
import arrow.mtl.typeclasses.MonadState
import arrow.mtl.typeclasses.MonadTrans
import arrow.mtl.typeclasses.MonadTransControl
import arrow.mtl.typeclasses.MonadWriter
import arrow.mtl.typeclasses.Run
import arrow.mtl.typeclasses.RunInBase
import arrow.mtl.typeclasses.StM
import arrow.mtl.typeclasses.StT
import arrow.mtl.typeclasses.defaultMonadBaseControl
import arrow.typeclasses.Alternative
import arrow.typeclasses.Applicative
import arrow.typeclasses.ApplicativeError
import arrow.typeclasses.Functor
import arrow.typeclasses.Monad
import arrow.typeclasses.MonadError
import arrow.typeclasses.Monoid

@extension
interface AccumTFunctor<S, F> : Functor<AccumTPartialOf<S, F>> {

  fun FF(): Functor<F>

  override fun <A, B> Kind<AccumTPartialOf<S, F>, A>.map(f: (A) -> B): Kind<AccumTPartialOf<S, F>, B> =
    this.fix().map(FF(), f)
}

@extension
interface AccumTApplicative<S, F> : Applicative<AccumTPartialOf<S, F>> {
  fun MS(): Monoid<S>
  fun MF(): Monad<F>

  override fun <A> just(a: A): Kind<AccumTPartialOf<S, F>, A> =
    AccumT.just(MS(), MF(), a)

  override fun <A, B> Kind<AccumTPartialOf<S, F>, A>.ap(ff: Kind<AccumTPartialOf<S, F>, (A) -> B>): Kind<AccumTPartialOf<S, F>, B> =
    fix().ap(MS(), MF(), ff)
}

@extension
interface AccumTMonad<S, F> : Monad<AccumTPartialOf<S, F>>, AccumTApplicative<S, F> {

  override fun MS(): Monoid<S>
  override fun MF(): Monad<F>

  override fun <A> just(a: A): Kind<AccumTPartialOf<S, F>, A> =
    AccumT.just(MS(), MF(), a)

  override fun <A, B> Kind<AccumTPartialOf<S, F>, A>.flatMap(f: (A) -> Kind<AccumTPartialOf<S, F>, B>): Kind<AccumTPartialOf<S, F>, B> =
    this.fix().flatMap(MS(), MF(), f)

  override fun <A, B> tailRecM(a: A, f: (A) -> Kind<AccumTPartialOf<S, F>, Either<A, B>>): Kind<AccumTPartialOf<S, F>, B> =
    AccumT.tailRecM(MF(), a, f)

  override fun <A, B> Kind<AccumTPartialOf<S, F>, A>.ap(ff: Kind<AccumTPartialOf<S, F>, (A) -> B>): Kind<AccumTPartialOf<S, F>, B> =
    fix().ap(MS(), MF(), ff)
}

@extension
interface AccumtTMonadTrans<S> : MonadTrans<Kind<ForAccumT, S>> {
  fun MS(): Monoid<S>
  override fun <G> monad(MG: Monad<G>): Monad<Kind<Kind<ForAccumT, S>, G>> = AccumT.monad(MS(), MG)

  override fun <G, A> Kind<G, A>.liftT(MG: Monad<G>): Kind2<Kind<ForAccumT, S>, G, A> =
    AccumT { _: S ->
      MG.run {
        map { a ->
          MS().empty() toT a
        }
      }
    }
}

@extension
interface AccumTMonadTransControl<S> : MonadTransControl<Kind<ForAccumT, S>> {

  fun MS(): Monoid<S>

  override fun <G> monad(MG: Monad<G>): Monad<Kind<Kind<ForAccumT, S>, G>> = AccumT.monad(MS(), MG)

  override fun <M, A> liftWith(MM: Monad<M>, runInM: (Run<Kind<ForAccumT, S>>) -> Kind<M, A>): Kind2<Kind<ForAccumT, S>, M, A> =
    AccumT { s ->
      MM.run {
        runInM(object : Run<Kind<ForAccumT, S>> {
          override fun <N, A> invoke(NM: Monad<N>, na: Kind2<Kind<ForAccumT, S>, N, A>): Kind<N, StT<Kind<ForAccumT, S>, A>> =
            NM.run {
              na.fix().runAccumT(s).map { StT<Kind<ForAccumT, S>, A>(it) }
            }
        }).map { a -> Tuple2(MS().empty(), a) }
      }
    }

  override fun <M, A> Kind<M, StT<Kind<ForAccumT, S>, A>>.restoreT(MM: Monad<M>): Kind2<Kind<ForAccumT, S>, M, A> =
    AccumT { _: S ->
      MM.run {
        map { it.st as Tuple2<S, A> }
      }
    }
}

fun <S, B, M> AccumT.Companion.monadBase(MBB: MonadBase<B, M>, MS: Monoid<S>): MonadBase<B, AccumTPartialOf<S, M>> =
  object : AccumTMonadBase<S, B, M> {
    override fun MBB(): MonadBase<B, M> = MBB
    override fun MS(): Monoid<S> = MS
  }

interface AccumTMonadBase<S, B, M> : MonadBase<B, AccumTPartialOf<S, M>> {
  fun MBB(): MonadBase<B, M>
  fun MS(): Monoid<S>
  override fun MB(): Monad<B> = MBB().MB()
  override fun MM(): Monad<AccumTPartialOf<S, M>> = AccumT.monad(MS(), MBB().MM())

  override fun <A> Kind<B, A>.liftBase(): Kind<AccumTPartialOf<S, M>, A> = MBB().run {
    AccumT.liftF(MS(), MBB().MM(), liftBase())
  }
}

fun <S, B, M> AccumT.Companion.monadBaseControl(MBB: MonadBaseControl<B, M>, MS: Monoid<S>): MonadBaseControl<B, AccumTPartialOf<S, M>> =
  object : AccumTMonadBaseControl<S, B, M> {
    override fun MBB(): MonadBaseControl<B, M> = MBB
    override fun MS(): Monoid<S> = MS
  }

interface AccumTMonadBaseControl<S, B, M> : MonadBaseControl<B, AccumTPartialOf<S, M>>, AccumTMonadBase<S, B, M> {
  override fun MBB(): MonadBaseControl<B, M>
  override fun MS(): Monoid<S>

  override fun <A> StM<AccumTPartialOf<S, M>, A>.restoreM(): Kind<AccumTPartialOf<S, M>, A> =
    defaultMonadBaseControl(MBB().MB(), MBB().MM(), AccumT.monadTransControl(MS()), MBB()).run { restoreM() }

  override fun <A> liftBaseWith(runInBase: (RunInBase<AccumTPartialOf<S, M>, B>) -> Kind<B, A>): Kind<AccumTPartialOf<S, M>, A> =
    defaultMonadBaseControl(MBB().MB(), MBB().MM(), AccumT.monadTransControl(MS()), MBB()).liftBaseWith(runInBase)

  override fun <A> Kind<B, A>.liftBase(): Kind<AccumTPartialOf<S, M>, A> = liftBaseWith { this }
}

@extension
interface AccumTAlternative<S, F> : Alternative<AccumTPartialOf<S, F>>, AccumTApplicative<S, F> {

  fun AF(): Alternative<F>
  override fun MF(): Monad<F>
  override fun MS(): Monoid<S>

  override fun <A> Kind<AccumTPartialOf<S, F>, A>.orElse(b: Kind<AccumTPartialOf<S, F>, A>): Kind<AccumTPartialOf<S, F>, A> =
    (this.fix() to b.fix()).let { (ls, rs) ->
      AccumT(AndThen.id<S>().flatMap { s ->
        AF().run {
          AndThen(ls.accumT).andThen { it.orElse(rs.runAccumT(s)) }
        }
      })
    }

  override fun <A> empty(): Kind<AccumTPartialOf<S, F>, A> =
    AccumT.liftF(MS(), AF(), AF().empty())
}

@extension
interface AccumTApplicativeError<S, F, E> : ApplicativeError<AccumTPartialOf<S, F>, E>, AccumTApplicative<S, F> {
  fun ME(): MonadError<F, E>

  override fun MS(): Monoid<S>
  override fun MF(): Monad<F> = ME()

  override fun <A> raiseError(e: E): Kind<AccumTPartialOf<S, F>, A> =
    AccumT.liftF(MS(), MF(), ME().raiseError(e))

  override fun <A> Kind<AccumTPartialOf<S, F>, A>.handleErrorWith(f: (E) -> Kind<AccumTPartialOf<S, F>, A>): Kind<AccumTPartialOf<S, F>, A> =
    this.fix().let { accumT ->
      AccumT(AndThen.id<S>().flatMap { s ->
        AndThen(accumT.accumT).andThen {
          ME().run {
            it.handleErrorWith { e ->
              f(e).fix().runAccumT(s)
            }
          }
        }
      })
    }
}

@extension
interface AccumTMonadError<S, F, E> : MonadError<AccumTPartialOf<S, F>, E>, AccumTApplicativeError<S, F, E>, AccumTMonad<S, F> {
  override fun MS(): Monoid<S>
  override fun ME(): MonadError<F, E>
  override fun MF(): Monad<F> = ME()
}

@extension
interface AccumTMonadState<S, W, F> : MonadState<AccumTPartialOf<W, F>, S>, AccumTMonad<W, F> {
  fun MSF(): MonadState<F, S>
  override fun MF(): Monad<F> = MSF()
  override fun MS(): Monoid<W>

  override fun get(): Kind<AccumTPartialOf<W, F>, S> =
    AccumT.liftF(MS(), MSF(), MSF().get())

  override fun set(s: S): Kind<AccumTPartialOf<W, F>, Unit> =
    AccumT.liftF(MS(), MSF(), MSF().set(s))
}

@extension
interface AccumTMonadReader<S, W, F> : MonadReader<AccumTPartialOf<W, F>, S>, AccumTMonad<W, F> {
  fun MR(): MonadReader<F, S>
  override fun MF(): Monad<F> = MR()
  override fun MS(): Monoid<W>

  override fun ask(): Kind<AccumTPartialOf<W, F>, S> =
    AccumT.liftF(MS(), MR(), MR().ask())

  override fun <A> Kind<AccumTPartialOf<W, F>, A>.local(f: (S) -> S): Kind<AccumTPartialOf<W, F>, A> =
    MR().run {
      AccumT(AndThen(fix().accumT).andThen { it.local(f) })
    }
}

@extension
interface AccumTMonadWriter<S, W, F> : MonadWriter<AccumTPartialOf<W, F>, S>, AccumTMonad<W, F> {
  fun MW(): MonadWriter<F, S>
  override fun MF(): Monad<F> = MW()
  override fun MS(): Monoid<W>

  override fun <A> writer(aw: Tuple2<S, A>): Kind<AccumTPartialOf<W, F>, A> =
    AccumT.liftF(MS(), MW(), MW().writer(aw))

  override fun <A> Kind<AccumTPartialOf<W, F>, A>.listen(): Kind<AccumTPartialOf<W, F>, Tuple2<S, A>> =
    MW().run {
      AccumT(AndThen(fix().accumT).andThen {
        it.listen().map { (w, sa) ->
          val (s, a) = sa
          Tuple2(s, Tuple2(w, a))
        }
      })
    }

  override fun <A> Kind<AccumTPartialOf<W, F>, Tuple2<(S) -> S, A>>.pass(): Kind<AccumTPartialOf<W, F>, A> =
    MW().run {
      AccumT(AndThen(fix().accumT).andThen {
        it.map { (s, fa) ->
          val (f, a) = fa
          Tuple2(f, Tuple2(s, a))
        }.pass()
      })
    }
}
