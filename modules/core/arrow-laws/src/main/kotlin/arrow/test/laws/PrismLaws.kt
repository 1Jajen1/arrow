package arrow.test.laws

import arrow.core.*
import arrow.core.extensions.const.applicative.applicative
import arrow.core.extensions.id.applicative.applicative
import arrow.optics.Prism
import arrow.optics.modify
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Const
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid
import arrow.typeclasses.value

object PrismLaws {

  fun <A, B> laws(prism: Prism<A, B>, arbA: Arbitrary<A>, arbB: Arbitrary<B>, funcGen: Arbitrary<(B) -> B>, EQA: Eq<A>, EQ_OPTION_B: Eq<Option<B>>): List<Law> = listOf(
    Law("Prism law: partial round trip one way", prism.partialRoundTripOneWay(arbA, EQA)),
    Law("Prism law: round trip other way", prism.roundTripOtherWay(arbB, EQ_OPTION_B)),
    Law("Prism law: modify identity", prism.modifyIdentity(arbA, EQA)),
    Law("Prism law: compose modify", prism.composeModify(arbA, funcGen, EQA)),
    Law("Prism law: consistent set modify", prism.consistentSetModify(arbA, arbB, EQA)),
    Law("Prism law: consistent modify with modifyF Id", prism.consistentModifyModifyFId(arbA, funcGen, EQA)),
    Law("Prism law: consistent get option modify id", prism.consistentGetOptionModifyId(arbA, EQ_OPTION_B))
  )

  fun <A, B> Prism<A, B>.partialRoundTripOneWay(arbA: Arbitrary<A>, EQA: Eq<A>): Property =
    forAll(arbA) { a ->
      getOrModify(a).fold(::identity, ::reverseGet)
        .eqv(a, EQA)
    }

  fun <A, B> Prism<A, B>.roundTripOtherWay(arbB: Arbitrary<B>, EQ_OPTION_B: Eq<Option<B>>): Property =
    forAll(arbB) { b ->
      getOption(reverseGet(b))
        .eqv(Some(b), EQ_OPTION_B)
    }

  fun <A, B> Prism<A, B>.modifyIdentity(arbA: Arbitrary<A>, EQA: Eq<A>): Property =
    forAll(arbA) { a ->
      modify(a, ::identity).eqv(a, EQA)
    }

  fun <A, B> Prism<A, B>.composeModify(arbA: Arbitrary<A>, funcGen: Arbitrary<(B) -> B>, EQA: Eq<A>): Property =
    forAll(Tuple3.arbitrary(arbA, funcGen, funcGen)) { (a, f, g) ->
      modify(modify(a, f), g).eqv(modify(a, g compose f), EQA)
    }

  fun <A, B> Prism<A, B>.consistentSetModify(arbA: Arbitrary<A>, arbB: Arbitrary<B>, EQA: Eq<A>): Property =
    forAll(Tuple2.arbitrary(arbA, arbB)) { (a, b) ->
      set(a, b).eqv(modify(a) { b }, EQA)
    }

  fun <A, B> Prism<A, B>.consistentModifyModifyFId(arbA: Arbitrary<A>, funcGen: Arbitrary<(B) -> B>, EQA: Eq<A>): Property =
    forAll(Tuple2.arbitrary(arbA, funcGen)) { (a, f) ->
      modifyF(Id.applicative(), a) { Id.just(f(it)) }.value().eqv(modify(a, f), EQA)
    }

  fun <A, B> Prism<A, B>.consistentGetOptionModifyId(arbA: Arbitrary<A>, EQ_OPTION_B: Eq<Option<B>>): Property =
    forAll(arbA) { a ->
      modifyF(Const.applicative(object : Monoid<Option<B>> {
        override fun Option<B>.combine(b: Option<B>): Option<B> = orElse { b }

        override fun empty(): Option<B> = None
      }), a) { Const(Some(it)) }.value().eqv(getOption(a), EQ_OPTION_B)
    }

}