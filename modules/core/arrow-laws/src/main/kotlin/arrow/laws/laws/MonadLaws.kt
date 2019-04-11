package arrow.laws.laws

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.monoid
import arrow.data.Kleisli
import arrow.free.Free
import arrow.free.bindingStackSafe
import arrow.free.run
import arrow.laws.generators.applicativeArbitrary
import arrow.laws.generators.functionAToBArbitrary
import arrow.laws.generators.makeFunAtoA
import arrow.propCheck.Property
import arrow.propCheck.and
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.either.arbitrary.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Monad
import arrow.typeclasses.Monoid
import arrow.typeclasses.Show
import kotlinx.coroutines.newSingleThreadContext

object MonadLaws {

  fun <F> laws(M: Monad<F>, EQ: Eq<Kind<F, Int>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> = laws(
    M,
    Int.arbitrary(),
    Int.monoid(),
    EQ, unsafeRun
  )

  fun <F, A> laws(M: Monad<F>, arbA: Arbitrary<A>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(M, arbA, MA, functionAToBArbitrary(arbA), EQ, unsafeRun)

  fun <F, A> laws(M: Monad<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(M, applicativeArbitrary(arbA, M), arbA, MA, arbAToA, arbAToA, EQ, unsafeRun)

  fun <F, A> laws(M: Monad<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(M, arbF, arbA, MA, arbAtoA, arbAtoA, EQ, unsafeRun)

  fun <F, A, B> laws(M: Monad<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(M, applicativeArbitrary(arbA, M), arbA, MA, arbAToB, arbBToA, EQ, unsafeRun)

  fun <F, A, B> laws(M: Monad<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> =
    SelectiveLaws.laws(M, arbF, arbA, MA, arbAToB, arbBToA, EQ) +
      listOf(
        Law("Monad Laws: left identity", M.leftIdentity(arbF, arbA, EQ)),
        Law("Monad Laws: right identity", M.rightIdentity(arbF, EQ)),
        Law("Monad Laws: kleisli left identity", M.kleisliLeftIdentity(arbF, arbA, EQ)),
        Law("Monad Laws: kleisli right identity", M.kleisliRightIdentity(arbF, arbA, EQ)),
        Law("Monad Laws: map / flatMap coherence", M.mapFlatMapCoherence(arbF, arbA, EQ)),
        Law("Monad Laws: monad comprehensions", M.monadComprehensions(arbA, makeFunAtoA(arbAToB, arbBToA), EQ)),
        Law("Monad Laws: monad comprehensions binding in other threads", M.monadComprehensionsBindInContext(arbA, EQ)),
        Law("Monad Laws: stack-safe//unsafe monad comprehensions equivalence", M.equivalentComprehensions(arbA, makeFunAtoA(arbAToB, arbBToA), EQ)),
        Law("Monad Laws: stack safe", M.stackSafety(5000, unsafeRun)),
        Law("Monad Laws: stack safe comprehensions", M.stackSafetyComprehensions(5000, unsafeRun)),
        Law("Monad Laws: selectM == select when Selective has a monad instance", M.selectEQSelectM(arbA, EQ))
      )

  fun <F, A> Monad<F>.leftIdentity(arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<A, Kind<F, A>>(arbF),
      arbA
    )) { (f, a) ->
      just(a).flatMap(f).eqv(f(a), EQ)
    }

  fun <F, A> Monad<F>.rightIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      fa.flatMap { just(it) }.eqv(fa, EQ)
    }

  fun <F, A> Monad<F>.kleisliLeftIdentity(arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property {
    val M = this
    return forAll(Tuple2.arbitrary(
      functionAToBArbitrary<A, Kind<F, A>>(arbF),
      arbA
    )) { (f, a) ->
      (Kleisli { n: A -> just(n) }.andThen(M, Kleisli(f)).run(a).eqv(f(a), EQ))
    }
  }

  fun <F, A> Monad<F>.kleisliRightIdentity(arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property {
    val M = this
    return forAll(Tuple2.arbitrary(
      functionAToBArbitrary<A, Kind<F, A>>(arbF),
      arbA
    )) { (f, a) ->
      (Kleisli(f).andThen(M, Kleisli { n: A -> just(n) }).run(a).eqv(f(a), EQ))
    }
  }

  fun <F, A> Monad<F>.mapFlatMapCoherence(arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<A, A>(arbA),
      arbF
    )) { (f, fa) ->
      fa.flatMap { just(f(it)) }.eqv(fa.map(f), EQ)
    }

  fun <F> Monad<F>.stackSafety(iterations: Int = 5000, unsafeRun: Kind<F, Int>.() -> Int): Property =
    tailRecM(0) { i -> just(if (i < iterations) Left(i + 1) else Right(i)) }
      .unsafeRun().eqv(iterations)

  fun <F> Monad<F>.stackSafetyComprehensions(iterations: Int = 5000, unsafeRun: Kind<F, Int>.() -> Int): Property =
    stackSafeTestProgram(0, iterations).run(this).unsafeRun()
      .eqv(iterations)

  fun <F, A> Monad<F>.equivalentComprehensions(arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property {
    val M = this
    return forAll(Tuple3.arbitrary(
      arbA,
      arbAToA,
      arbAToA
    )) { (a, f, g) ->
      val aa = binding {
        val (a) = just(a)
        val (b) = just(f(a))
        val (c) = just(g(b))
        c
      }

      val bb = bindingStackSafe {
        val (a) = just(a)
        val (b) = just(f(a))
        val (c) = just(g(b))
        c
      }.run(M)

      // TODO There is a problem with Show for IO here that crashes the thing on toString using Show.any() or Show.fromToString()
      aa.eqv(bb, EQ, Show { "" }) and Eval.later {
        aa.eqv(just(g(f(a))), EQ, Show { "" })
      }
    }
  }

  fun <F, A> Monad<F>.monadComprehensions(arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple3.arbitrary(
      arbA,
      arbAToA,
      arbAToA
    )) { (a, f, g) ->
      binding {
        val (a) = just(a)
        val (b) = just(f(a))
        val (c) = just(g(b))
        c
      }.eqv(just(g(f(a))), EQ)
    }

  fun <F, A> Monad<F>.selectEQSelectM(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Either.arbitrary(arbA, arbA)) { either ->
      val f = just<(A) -> A>(::identity)
      just(either).select(f).eqv(just(either).selectM(f), EQ)
    }

  fun <F, A> Monad<F>.monadComprehensionsBindInContext(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbA) { a ->
      binding {
        val a = bindIn(newSingleThreadContext("1")) { a }
        val b = bindIn(newSingleThreadContext("2")) { a }
        b
      }.eqv(just(a), EQ)
    }

  fun <F> Monad<F>.stackSafeTestProgram(n: Int, stopAt: Int): Free<F, Int> = bindingStackSafe {
    val (v) = this.just(n + 1)
    val r = if (v < stopAt) stackSafeTestProgram(v, stopAt).bind() else this.just(v).bind()
    r
  }
}
