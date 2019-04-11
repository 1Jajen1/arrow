package arrow.laws.generators

import arrow.Kind
import arrow.core.Left
import arrow.core.Right
import arrow.core.andThen
import arrow.core.toT
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.arbitrary.Gen
import arrow.propCheck.arbitrary.gen.monad.flatMap
import arrow.propCheck.instances.arbitrary
import arrow.typeclasses.Applicative
import arrow.typeclasses.ApplicativeError
import kotlin.math.max

fun <A> smallRangeArb(AA: Arbitrary<A>): Arbitrary<A> = Arbitrary(
  AA.arbitrary().scale { max(0, it / 10) }
) { fail -> AA.shrink(fail) }

fun <F, A> applicativeArbitrary(AA: Arbitrary<A>, AP: Applicative<F>): Arbitrary<Kind<F, A>> = Arbitrary(
  AA.arbitrary().map { AP.just(it) }
)

fun <F, A, E> applicativeErrorArbitrary(AA: Arbitrary<A>, AE: Arbitrary<E>, AP: ApplicativeError<F, E>): Arbitrary<Kind<F, A>> = Arbitrary(
  Gen.frequency(
    3 toT AA.arbitrary().map(::Right),
    1 toT AE.arbitrary().map(::Left)
  ).map { it.fold(AP::raiseError, AP::just) }
)

// TODO replace as soon as propCheck supports something like CoArbitrary or Function
fun <A, B> functionAToBArbitrary(AB: Arbitrary<B>): Arbitrary<(A) -> B> = Arbitrary(
  AB.arbitrary().map { { a: A -> it } }
)

fun <A, B> makeFunAtoA(arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>): Arbitrary<(A) -> A> = Arbitrary(
  arbAtoB.arbitrary().flatMap { f -> arbBtoA.arbitrary().map { f andThen it } }
)

object ThrowableArbitrary : Arbitrary<Throwable> {
  override fun arbitrary(): Gen<Throwable> = Gen.oneOf(
    NonFatalThrowableArbitrary.arbitrary(),
    FatalThrowableArbitrary.arbitrary()
  )
}

object NonFatalThrowableArbitrary : Arbitrary<Throwable> {
  override fun arbitrary(): Gen<Throwable> =
    Gen.elements(RuntimeException(), NoSuchElementException(), IllegalArgumentException())
}

object FatalThrowableArbitrary : Arbitrary<Throwable> {
  override fun arbitrary(): Gen<Throwable> =
    Gen.elements(ThreadDeath(), StackOverflowError(), OutOfMemoryError(), InterruptedException())
}