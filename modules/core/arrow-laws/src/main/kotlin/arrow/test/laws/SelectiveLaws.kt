package arrow.test.laws

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.monoid
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.either.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.test.generators.applicativeArbitrary
import arrow.test.generators.functionAToBArbitrary
import arrow.test.generators.makeFunAtoA
import arrow.typeclasses.Applicative
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid
import arrow.typeclasses.Selective

object SelectiveLaws {

  fun <F> laws(S: Selective<F>, EQ: Eq<Kind<F, Int>>): List<Law> = laws(
    S, Int.arbitrary(), Int.monoid(), functionAToBArbitrary(Int.arbitrary()), EQ
  )

  fun <F, A> laws(S: Selective<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(S, applicativeArbitrary(arbA, S), arbA, MA, arbAToA, arbAToA, EQ)

  fun <F, A> laws(S: Selective<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(S, arbF, arbA, MA, arbAtoA, arbAtoA, EQ)

  fun <F, A, B> laws(S: Selective<F>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(S, applicativeArbitrary(arbA, S), arbA, MA, arbAToB, arbBToA, EQ)

  fun <F, A, B> laws(S: Selective<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, MA: Monoid<A>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>): List<Law> =
    ApplicativeLaws.laws(S, arbF, arbA, MA, arbAtoB, arbBtoA, EQ) + listOf(
      Law("Selective Laws: identity", S.identityLaw(arbA, EQ)),
      Law("Selective Laws: distributivity", S.distributivity(arbA, makeFunAtoA(arbAtoB, arbBtoA), EQ)),
      Law("Selective Laws: associativity", S.associativity(arbA, makeFunAtoA(arbAtoB, arbBtoA), EQ)),
      Law("Selective Laws: branch", S.branch(arbA, makeFunAtoA(arbAtoB, arbBtoA), EQ)),
      Law("Selective Laws: ifS", S.ifSLaw(arbA, EQ))
    )

  fun <F, A> Selective<F>.identityLaw(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Either.arbitrary(arbA, arbA)) { either ->
      either.fold(
        { l -> just(either).select(just(::identity)).eqv(just(l), EQ) },
        { r -> just(either).select(just(::identity)).eqv(just(r), EQ) }
      )
    }

  fun <F, A, B> Applicative<F>.sequenceRight(fa: Kind<F, A>, fb: Kind<F, B>): Kind<F, B> =
    fa.product(fb).map { (_, b) -> b }

  fun <F, A> Selective<F>.distributivity(arbA: Arbitrary<A>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(
      Tuple3.arbitrary(
        Either.arbitrary(arbA, arbA),
        arbAtoA,
        arbAtoA
      )
    ) { (either, ab1, ab2) ->
      val fe = just(either)
      val f = just(ab1)
      val g = just(ab2)
      fe.select(sequenceRight(f, g)).eqv(sequenceRight(fe.select(f), fe.select(g)), EQ)
    }


  fun <F, A> Selective<F>.associativity(arbA: Arbitrary<A>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(
      Tuple3.arbitrary(
        arbA,
        arbAtoA,
        arbAtoA
      )
    ) { (a, ab1, ab2) ->

      val x: Kind<F, Either<A, A>> = just(a.right())
      val y: Kind<F, Either<A, (A) -> A>> = just(ab1.right())
      val z: Kind<F, (A) -> (A) -> A> = just({ _: A -> ab2 })

      val p: Kind<F, Either<A, Either<Tuple2<A, A>, A>>> = x.map { e -> e.map(::Right) }
      val q: Kind<F, (A) -> Either<Tuple2<A, A>, A>> =
        y.map { e -> { i: A -> e.bimap({ l -> l toT i }, { r -> r(i) }) } }
      val r: Kind<F, (Tuple2<A, A>) -> A> = z.map { { (t1, t2): Tuple2<A, A> -> it(t1)(t2) } }
      x.select(y.select(z)).eqv(p.select(q).select(r), EQ)
    }

  fun <F, A> Selective<F>.branch(arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(
      Tuple3.arbitrary(
        arbAToA,
        arbAToA,
        Either.arbitrary(arbA, arbA)
      )
    ) { (di, fi, either) ->
      val fl = just(di)
      val fr = just(fi)
      either.fold(
        { l -> just(either).branch(fl, fr).eqv(fl.map { ff -> ff(l) }, EQ) },
        { r -> just(either).branch(fl, fr).eqv(fr.map { ff -> ff(r) }, EQ) }
      )
    }

  fun <F, A> Selective<F>.ifSLaw(arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple3.arbitrary(
      Boolean.arbitrary(),
      arbA,
      arbA
    )) { (bool, l, r) ->
      if (bool) just(bool).ifS(just(l), just(r)).eqv(just(l), EQ)
      else just(bool).ifS(just(l), just(r)).eqv(just(r), EQ)
    }

}
