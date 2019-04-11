package arrow.laws.laws

import arrow.core.*
import arrow.core.extensions.const.applicative.applicative
import arrow.core.extensions.id.functor.functor
import arrow.laws.generators.functionAToBArbitrary
import arrow.optics.Iso
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.propCheck.property.testable.testable
import arrow.typeclasses.Const
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid
import arrow.typeclasses.value

object IsoLaws {

  fun <A, B> laws(iso: Iso<A, B>, arbA: Arbitrary<A>, arbB: Arbitrary<B>, EQA: Eq<A>, EQB: Eq<B>, MB: Monoid<B>): List<Law> =
    laws(iso, arbA, arbB, functionAToBArbitrary(arbB), EQA, EQB, MB)

  fun <A, B> laws(iso: Iso<A, B>, arbA: Arbitrary<A>, arbB: Arbitrary<B>, arbBToB: Arbitrary<(B) -> B>, EQA: Eq<A>, EQB: Eq<B>, MB: Monoid<B>): List<Law> =
    listOf(
      Law("Iso Law: round trip one way", iso.roundTripOneWay(arbA, EQA)),
      Law("Iso Law: round trip other way", iso.roundTripOtherWay(arbB, EQB)),
      Law("Iso Law: modify identity is identity", iso.modifyIdentity(arbA, EQA)),
      Law("Iso Law: compose modify", iso.composeModify(arbA, arbBToB, EQA)),
      Law("Iso Law: consitent set with modify", iso.consistentSetModify(arbA, arbB, EQA)),
      Law("Iso Law: consistent modify with modify identity", iso.consistentModifyModifyId(arbA, arbBToB, EQA)),
      Law("Iso Law: consitent get with modify identity", iso.consitentGetModifyId(arbA, EQB, MB))
    )

  fun <A, B> Iso<A, B>.roundTripOneWay(aGen: Arbitrary<A>, EQA: Eq<A>): Property =
    forAll(aGen.arbitrary(), Property.testable()) { a ->
      reverseGet(get(a)).eqv(a, EQA)
    }

  fun <A, B> Iso<A, B>.roundTripOtherWay(bGen: Arbitrary<B>, EQB: Eq<B>): Property =
    forAll(bGen.arbitrary(), Property.testable()) { b ->
      get(reverseGet(b)).eqv(b, EQB)
    }

  fun <A, B> Iso<A, B>.modifyIdentity(aGen: Arbitrary<A>, EQA: Eq<A>): Property =
    forAll(aGen.arbitrary(), Property.testable()) { a ->
      modify(a, ::identity).eqv(a, EQA)
    }

  fun <A, B> Iso<A, B>.composeModify(aGen: Arbitrary<A>, funcGen: Arbitrary<(B) -> B>, EQA: Eq<A>): Property =
    forAll(Tuple3.arbitrary(aGen, funcGen, funcGen)) { (a, f, g) ->
      modify(modify(a, f), g).eqv(modify(a, g compose f), EQA)
    }

  fun <A, B> Iso<A, B>.consistentSetModify(aGen: Arbitrary<A>, bGen: Arbitrary<B>, EQA: Eq<A>): Property =
    forAll(Tuple2.arbitrary(aGen, bGen)) { (a, b) ->
      set(b).eqv(modify(a) { b }, EQA)
    }

  fun <A, B> Iso<A, B>.consistentModifyModifyId(aGen: Arbitrary<A>, funcGen: Arbitrary<(B) -> B>, EQA: Eq<A>): Property =
    forAll(Tuple2.arbitrary(aGen, funcGen)) { (a, f) ->
      modify(a, f).eqv(modifyF(Id.functor(), a) { Id.just(f(it)) }.value(), EQA)
    }

  fun <A, B> Iso<A, B>.consitentGetModifyId(aGen: Arbitrary<A>, EQB: Eq<B>, bMonoid: Monoid<B>): Property =
    forAll(aGen.arbitrary(), Property.testable()) { a ->
      get(a).eqv(modifyF(Const.applicative(bMonoid), a, ::Const).value(), EQB)
    }
}