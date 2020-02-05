package arrow.mtl.extensions

import arrow.Kind
import arrow.core.AndThen
import arrow.core.AndThenPartialOf
import arrow.core.Either
import arrow.core.EitherPartialOf
import arrow.core.Eval
import arrow.core.ForEval
import arrow.core.ForFunction0
import arrow.core.ForId
import arrow.core.ForListK
import arrow.core.ForNonEmptyList
import arrow.core.ForOption
import arrow.core.ForSequenceK
import arrow.core.Function0
import arrow.core.Function1
import arrow.core.Function1PartialOf
import arrow.core.Id
import arrow.core.Ior
import arrow.core.IorPartialOf
import arrow.core.ListK
import arrow.core.MapK
import arrow.core.MapKPartialOf
import arrow.core.Nel
import arrow.core.NonEmptyList
import arrow.core.Option
import arrow.core.SequenceK
import arrow.core.extensions.andthen.monad.monad
import arrow.core.extensions.either.monad.monad
import arrow.core.extensions.eval.monad.monad
import arrow.core.extensions.function0.monad.monad
import arrow.core.extensions.function1.monad.monad
import arrow.core.extensions.id.monad.monad
import arrow.core.extensions.ior.monad.monad
import arrow.core.extensions.listk.monad.monad
import arrow.core.extensions.nonemptylist.monad.monad
import arrow.core.extensions.option.monad.monad
import arrow.core.extensions.sequencek.monad.monad
import arrow.core.fix
import arrow.core.right
import arrow.core.rightIor
import arrow.extension
import arrow.mtl.typeclasses.MonadBase
import arrow.mtl.typeclasses.MonadBaseControl
import arrow.mtl.typeclasses.RunInBase
import arrow.mtl.typeclasses.StM
import arrow.typeclasses.Monad
import arrow.typeclasses.Semigroup

// ListK
@extension
interface ListKMonadBase : MonadBase<ForListK, ForListK> {
  override fun MB(): Monad<ForListK> = ListK.monad()
  override fun MM(): Monad<ForListK> = ListK.monad()
  override fun <A> Kind<ForListK, A>.liftBase(): Kind<ForListK, A> = this
}

@extension
interface ListKMonadBaseControl : MonadBaseControl<ForListK, ForListK>, ListKMonadBase {
  override fun <A> liftBaseWith(runInBase: (RunInBase<ForListK, ForListK>) -> Kind<ForListK, A>): Kind<ForListK, A> =
    runInBase(object : RunInBase<ForListK, ForListK> {
      override fun <A> invoke(ma: Kind<ForListK, A>): Kind<ForListK, StM<ForListK, A>> =
        ma.fix().map { StM<ForListK, A>(it) }
    })

  override fun <A> StM<ForListK, A>.restoreM(): Kind<ForListK, A> = ListK.just(st as A)

  override fun <A> Kind<ForListK, A>.liftBase(): Kind<ForListK, A> = this
}

// Option
@extension
interface OptionMonadBase : MonadBase<ForOption, ForOption> {
  override fun MB(): Monad<ForOption> = Option.monad()
  override fun MM(): Monad<ForOption> = Option.monad()
  override fun <A> Kind<ForOption, A>.liftBase(): Kind<ForOption, A> = this
}

@extension
interface OptionMonadBaseControl : MonadBaseControl<ForOption, ForOption>, OptionMonadBase {
  override fun <A> liftBaseWith(runInBase: (RunInBase<ForOption, ForOption>) -> Kind<ForOption, A>): Kind<ForOption, A> =
    runInBase(object : RunInBase<ForOption, ForOption> {
      override fun <A> invoke(ma: Kind<ForOption, A>): Kind<ForOption, StM<ForOption, A>> =
        ma.fix().map { StM<ForOption, A>(it) }
    })

  override fun <A> StM<ForOption, A>.restoreM(): Kind<ForOption, A> = Option.just(st as A)

  override fun <A> Kind<ForOption, A>.liftBase(): Kind<ForOption, A> = this
}

// Id
@extension
interface IdMonadBase : MonadBase<ForId, ForId> {
  override fun MB(): Monad<ForId> = Id.monad()
  override fun MM(): Monad<ForId> = Id.monad()
  override fun <A> Kind<ForId, A>.liftBase(): Kind<ForId, A> = this
}

@extension
interface IdMonadBaseControl : MonadBaseControl<ForId, ForId>, IdMonadBase {
  override fun <A> liftBaseWith(runInBase: (RunInBase<ForId, ForId>) -> Kind<ForId, A>): Kind<ForId, A> =
    runInBase(object : RunInBase<ForId, ForId> {
      override fun <A> invoke(ma: Kind<ForId, A>): Kind<ForId, StM<ForId, A>> =
        ma.fix().map { StM<ForId, A>(it) }
    })

  override fun <A> StM<ForId, A>.restoreM(): Kind<ForId, A> = Id(st as A)

  override fun <A> Kind<ForId, A>.liftBase(): Kind<ForId, A> = this
}

// Either
@extension
interface EitherMonadBase<E> : MonadBase<EitherPartialOf<E>, EitherPartialOf<E>> {
  override fun MB(): Monad<EitherPartialOf<E>> = Either.monad()
  override fun MM(): Monad<EitherPartialOf<E>> = Either.monad()
  override fun <A> Kind<EitherPartialOf<E>, A>.liftBase(): Kind<EitherPartialOf<E>, A> = this
}

@extension
interface EitherMonadBaseControl<E> : MonadBaseControl<EitherPartialOf<E>, EitherPartialOf<E>>, EitherMonadBase<E> {
  override fun <A> liftBaseWith(runInBase: (RunInBase<EitherPartialOf<E>, EitherPartialOf<E>>) -> Kind<EitherPartialOf<E>, A>): Kind<EitherPartialOf<E>, A> =
    runInBase(object : RunInBase<EitherPartialOf<E>, EitherPartialOf<E>> {
      override fun <A> invoke(ma: Kind<EitherPartialOf<E>, A>): Kind<EitherPartialOf<E>, StM<EitherPartialOf<E>, A>> =
        ma.fix().map { StM<EitherPartialOf<E>, A>(it) }
    })

  override fun <A> StM<EitherPartialOf<E>, A>.restoreM(): Kind<EitherPartialOf<E>, A> = (st as A).right()

  override fun <A> Kind<EitherPartialOf<E>, A>.liftBase(): Kind<EitherPartialOf<E>, A> = this
}

// Ior
@extension
interface IorMonadBase<L> : MonadBase<IorPartialOf<L>, IorPartialOf<L>> {
  fun SL(): Semigroup<L>
  override fun MB(): Monad<IorPartialOf<L>> = Ior.monad(SL())
  override fun MM(): Monad<IorPartialOf<L>> = Ior.monad(SL())
  override fun <A> Kind<IorPartialOf<L>, A>.liftBase(): Kind<IorPartialOf<L>, A> = this
}

@extension
interface IorMonadBaseControl<L> : MonadBaseControl<IorPartialOf<L>, IorPartialOf<L>>, IorMonadBase<L> {
  override fun SL(): Semigroup<L>

  override fun <A> liftBaseWith(runInBase: (RunInBase<IorPartialOf<L>, IorPartialOf<L>>) -> Kind<IorPartialOf<L>, A>): Kind<IorPartialOf<L>, A> =
    runInBase(object : RunInBase<IorPartialOf<L>, IorPartialOf<L>> {
      override fun <A> invoke(ma: Kind<IorPartialOf<L>, A>): Kind<IorPartialOf<L>, StM<IorPartialOf<L>, A>> =
        ma.fix().map { StM<IorPartialOf<L>, A>(it) }
    })

  override fun <A> StM<IorPartialOf<L>, A>.restoreM(): Kind<IorPartialOf<L>, A> = (st as A).rightIor()

  override fun <A> Kind<IorPartialOf<L>, A>.liftBase(): Kind<IorPartialOf<L>, A> = this
}

// AndThen
@extension
interface AndThenMonadBase<I> : MonadBase<AndThenPartialOf<I>, AndThenPartialOf<I>> {
  override fun MB(): Monad<AndThenPartialOf<I>> = AndThen.monad()
  override fun MM(): Monad<AndThenPartialOf<I>> = AndThen.monad()
  override fun <A> Kind<AndThenPartialOf<I>, A>.liftBase(): Kind<AndThenPartialOf<I>, A> = this
}

@extension
interface AndThenMonadBaseControl<I> : MonadBaseControl<AndThenPartialOf<I>, AndThenPartialOf<I>>, AndThenMonadBase<I> {
  override fun <A> liftBaseWith(runInBase: (RunInBase<AndThenPartialOf<I>, AndThenPartialOf<I>>) -> Kind<AndThenPartialOf<I>, A>): Kind<AndThenPartialOf<I>, A> =
    runInBase(object : RunInBase<AndThenPartialOf<I>, AndThenPartialOf<I>> {
      override fun <A> invoke(ma: Kind<AndThenPartialOf<I>, A>): Kind<AndThenPartialOf<I>, StM<AndThenPartialOf<I>, A>> =
        ma.fix().andThen { StM<AndThenPartialOf<I>, A>(it) }
    })

  override fun <A> StM<AndThenPartialOf<I>, A>.restoreM(): Kind<AndThenPartialOf<I>, A> =
    AndThen { (st as A) }

  override fun <A> Kind<AndThenPartialOf<I>, A>.liftBase(): Kind<AndThenPartialOf<I>, A> = this
}

// Eval
@extension
interface EvalMonadBase : MonadBase<ForEval, ForEval> {
  override fun MB(): Monad<ForEval> = Eval.monad()
  override fun MM(): Monad<ForEval> = Eval.monad()
  override fun <A> Kind<ForEval, A>.liftBase(): Kind<ForEval, A> = this
}

@extension
interface EvalMonadBaseControl : MonadBaseControl<ForEval, ForEval>, EvalMonadBase {
  override fun <A> liftBaseWith(runInBase: (RunInBase<ForEval, ForEval>) -> Kind<ForEval, A>): Kind<ForEval, A> =
    runInBase(object : RunInBase<ForEval, ForEval> {
      override fun <A> invoke(ma: Kind<ForEval, A>): Kind<ForEval, StM<ForEval, A>> =
        ma.fix().map { StM<ForEval, A>(it) }
    })

  override fun <A> StM<ForEval, A>.restoreM(): Kind<ForEval, A> = Eval.now(st as A)

  override fun <A> Kind<ForEval, A>.liftBase(): Kind<ForEval, A> = this
}

// Function0
@extension
interface Function0MonadBase : MonadBase<ForFunction0,ForFunction0> {
  override fun MB(): Monad<ForFunction0> = Function0.monad()
  override fun MM(): Monad<ForFunction0> = Function0.monad()
  override fun <A> Kind<ForFunction0, A>.liftBase(): Kind<ForFunction0, A> = this
}

@extension
interface Function0MonadBaseControl : MonadBaseControl<ForFunction0, ForFunction0>, Function0MonadBase {
  override fun <A> liftBaseWith(runInBase: (RunInBase<ForFunction0, ForFunction0>) -> Kind<ForFunction0, A>): Kind<ForFunction0, A> =
    runInBase(object : RunInBase<ForFunction0, ForFunction0> {
      override fun <A> invoke(ma: Kind<ForFunction0, A>): Kind<ForFunction0, StM<ForFunction0, A>> =
        ma.fix().map { StM<ForFunction0, A>(it) }
    })

  override fun <A> StM<ForFunction0, A>.restoreM(): Kind<ForFunction0, A> = Function0 { (st as A) }

  override fun <A> Kind<ForFunction0, A>.liftBase(): Kind<ForFunction0, A> = this
}

// Function1
@extension
interface Function1MonadBase<I> : MonadBase<Function1PartialOf<I>, Function1PartialOf<I>> {
  override fun MB(): Monad<Function1PartialOf<I>> = Function1.monad()
  override fun MM(): Monad<Function1PartialOf<I>> = Function1.monad()
  override fun <A> Kind<Function1PartialOf<I>, A>.liftBase(): Kind<Function1PartialOf<I>, A> = this
}

@extension
interface Function1MonadBaseControl<I> : MonadBaseControl<Function1PartialOf<I>, Function1PartialOf<I>>, Function1MonadBase<I> {
  override fun <A> liftBaseWith(runInBase: (RunInBase<Function1PartialOf<I>, Function1PartialOf<I>>) -> Kind<Function1PartialOf<I>, A>): Kind<Function1PartialOf<I>, A> =
    runInBase(object : RunInBase<Function1PartialOf<I>, Function1PartialOf<I>> {
      override fun <A> invoke(ma: Kind<Function1PartialOf<I>, A>): Kind<Function1PartialOf<I>, StM<Function1PartialOf<I>, A>> =
        ma.fix().map { StM<Function1PartialOf<I>, A>(it) }
    })

  override fun <A> StM<Function1PartialOf<I>, A>.restoreM(): Kind<Function1PartialOf<I>, A> = Function1 { (st as A) }

  override fun <A> Kind<Function1PartialOf<I>, A>.liftBase(): Kind<Function1PartialOf<I>, A> = this
}

// Nel
@extension
interface NonEmptyListMonadBase : MonadBase<ForNonEmptyList, ForNonEmptyList> {
  override fun MB(): Monad<ForNonEmptyList> = NonEmptyList.monad()
  override fun MM(): Monad<ForNonEmptyList> = NonEmptyList.monad()
  override fun <A> Kind<ForNonEmptyList, A>.liftBase(): Kind<ForNonEmptyList, A> = this
}

@extension
interface NonEmptyListMonadBaseControl : MonadBaseControl<ForNonEmptyList, ForNonEmptyList>, NonEmptyListMonadBase {
  override fun <A> liftBaseWith(runInBase: (RunInBase<ForNonEmptyList, ForNonEmptyList>) -> Kind<ForNonEmptyList, A>): Kind<ForNonEmptyList, A> =
    runInBase(object : RunInBase<ForNonEmptyList, ForNonEmptyList> {
      override fun <A> invoke(ma: Kind<ForNonEmptyList, A>): Kind<ForNonEmptyList, StM<ForNonEmptyList, A>> =
        ma.fix().map { StM<ForNonEmptyList, A>(it) }
    })

  override fun <A> StM<ForNonEmptyList, A>.restoreM(): Kind<ForNonEmptyList, A> = Nel(st as A)

  override fun <A> Kind<ForNonEmptyList, A>.liftBase(): Kind<ForNonEmptyList, A> = this
}

// sequence
@extension
interface SequenceKMonadBase : MonadBase<ForSequenceK, ForSequenceK> {
  override fun MB(): Monad<ForSequenceK> = SequenceK.monad()
  override fun MM(): Monad<ForSequenceK> = SequenceK.monad()
  override fun <A> Kind<ForSequenceK, A>.liftBase(): Kind<ForSequenceK, A> = this
}

@extension
interface SequenceKMonadBaseControl : MonadBaseControl<ForSequenceK, ForSequenceK>, SequenceKMonadBase {
  override fun <A> liftBaseWith(runInBase: (RunInBase<ForSequenceK, ForSequenceK>) -> Kind<ForSequenceK, A>): Kind<ForSequenceK, A> =
    runInBase(object : RunInBase<ForSequenceK, ForSequenceK> {
      override fun <A> invoke(ma: Kind<ForSequenceK, A>): Kind<ForSequenceK, StM<ForSequenceK, A>> =
        ma.fix().map { StM<ForSequenceK, A>(it) }
    })

  override fun <A> StM<ForSequenceK, A>.restoreM(): Kind<ForSequenceK, A> = SequenceK.just(st as A)

  override fun <A> Kind<ForSequenceK, A>.liftBase(): Kind<ForSequenceK, A> = this
}
