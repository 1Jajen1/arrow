package arrow.laws.laws

import arrow.Kind
import arrow.core.Tuple2
import arrow.data.Cokleisli
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.laws.generators.functionAToBArbitrary
import arrow.laws.generators.makeFunAtoA
import arrow.typeclasses.Comonad
import arrow.typeclasses.Eq
import arrow.typeclasses.cobinding

object ComonadLaws {

  fun <F, A> laws(CM: Comonad<F>, arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(CM, arbF, arbAToA, arbAToA, EQ)

  fun <F, A, B> laws(
    CM: Comonad<F>,
    arbF: Arbitrary<Kind<F, A>>,
    arbAToB: Arbitrary<(A) -> B>,
    arbBToA: Arbitrary<(B) -> A>,
    EQ: Eq<Kind<F, A>>
  ): List<Law> =
    FunctorLaws.laws(CM, arbF, arbAToB, arbBToA, EQ) + listOf(
      Law("Comonad Laws: duplicate then extract is identity", CM.duplicateThenExtractIsId(arbF, EQ)),
      Law("Comonad Laws: duplicate then map into extract is identity", CM.duplicateThenMapExtractIsId(arbF, EQ)),
      Law("Comonad Laws: map and coflatMap are coherent", CM.mapAndCoflatmapCoherence(arbF, makeFunAtoA(arbAToB, arbBToA), EQ)),
      Law("Comonad Laws: left identity", CM.comonadLeftIdentity(arbF, EQ)),
      Law("Comonad Laws: right identity", CM.comonadRightIdentity(arbF, EQ)),
      Law("Comonad Laws: cokleisli left identity", CM.cokleisliLeftIdentity(arbF, EQ)),
      Law("Comonad Laws: cokleisli right identity", CM.cokleisliRightIdentity(arbF, EQ)),
      Law("Comonad Laws: cobinding", CM.cobinding(arbF, makeFunAtoA(arbAToB, arbBToA), EQ))
    )

  fun <F, A> Comonad<F>.duplicateThenExtractIsId(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      fa.duplicate().extract().eqv(fa, EQ)
    }

  fun <F, A> Comonad<F>.duplicateThenMapExtractIsId(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      fa.duplicate().map { it.extract() }.eqv(fa, EQ)
    }

  fun <F, A> Comonad<F>.mapAndCoflatmapCoherence(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      arbF,
      arbAToA
    )) { (fa, f) ->
      fa.map(f).eqv(fa.coflatMap { f(it.extract()) }, EQ)
    }

  fun <F, A> Comonad<F>.comonadLeftIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      fa.coflatMap { it.extract() }.eqv(fa, EQ)
    }

  fun <F, A> Comonad<F>.comonadRightIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      arbF,
      functionAToBArbitrary<Kind<F, A>, Kind<F, A>>(arbF)
    )) { (fa, f) ->
      fa.coflatMap(f).extract().eqv(f(fa), EQ)
    }

  fun <F, A> Comonad<F>.cokleisliLeftIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property {
    val MM = this
    return forAll(Tuple2.arbitrary(
      arbF,
      functionAToBArbitrary<Kind<F, A>, Kind<F, A>>(arbF)
    )) { (fa, f) ->
      Cokleisli(MM) { hk: Kind<F, A> -> hk.extract() }.andThen(Cokleisli(MM, f)).run(fa).eqv(f(fa), EQ)
    }
  }

  fun <F, A> Comonad<F>.cokleisliRightIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property {
    val MM = this
    return forAll(Tuple2.arbitrary(
      arbF,
      functionAToBArbitrary<Kind<F, A>, Kind<F, A>>(arbF)
    )) { (fa, f) ->
      Cokleisli(MM, f).andThen(Cokleisli(MM) { hk: Kind<F, Kind<F, A>> -> hk.extract() }).run(fa).eqv(f(fa), EQ)
    }
  }

  fun <F, A> Comonad<F>.cobinding(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      arbF,
      arbAToA
    )) { (fa, f) ->
      cobinding {
        val x = fa.extract()
        val y = extract { fa.map(f) }
        fa.map(f)
      }.eqv(fa.map { f(f(it)) }, EQ)
    }
}
