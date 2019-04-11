package arrow.laws.laws

import arrow.core.*
import arrow.core.extensions.const.applicative.applicative
import arrow.core.extensions.id.applicative.applicative
import arrow.laws.generators.functionAToBArbitrary
import arrow.optics.Optional
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.arbitrary.Gen
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.propCheck.instances.tuple4.arbitrary.arbitrary
import arrow.typeclasses.Const
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid
import arrow.typeclasses.value

object OptionalLaws {

  fun <A, B> laws(
    arbOptional: Arbitrary<Optional<A, B>>,
    arbA: Arbitrary<A>,
    arbB: Arbitrary<B>,
    EQ: Eq<A>,
    EQ_OPTION_B: Eq<Option<B>>
  ): List<Law> = laws(arbOptional, arbA, arbB, functionAToBArbitrary(arbB), EQ, EQ_OPTION_B)

  fun <A, B> laws(
    arbOptional: Arbitrary<Optional<A, B>>,
    arbA: Arbitrary<A>,
    arbB: Arbitrary<B>,
    arbBToB: Arbitrary<(B) -> B>,
    EQA: Eq<A>,
    EQ_OPTION_B: Eq<Option<B>>
  ): List<Law> = listOf(
    Law("Optional Law: set what you get", getOptionSet(arbOptional, arbA, EQA)),
    Law("Optional Law: set what you get", setGetOption(arbOptional, arbA, arbB, EQ_OPTION_B)),
    Law("Optional Law: set is idempotent", setIdempotent(arbOptional, arbA, arbB, EQA)),
    Law("Optional Law: modify identity = identity", modifyIdentity(arbOptional, arbA, EQA)),
    Law("Optional Law: compose modify", composeModify(arbOptional, arbA, arbBToB, EQA)),
    Law("Optional Law: consistent set with modify", consistentSetModify(arbOptional, arbA, arbB, EQA)),
    Law("Optional Law: consistent modify with modify identity",
      consistentModifyModifyId(
        arbOptional,
        arbA,
        arbBToB,
        EQA
      )
    ),
    Law("Optional Law: consistent getOption with modify identity",
      consistentGetOptionModifyId(
        arbOptional,
        arbA,
        EQ_OPTION_B
      )
    )
  )

  /**
   * Warning: Use only when a `Gen.constant()` applies
   */
  fun <A, B> laws(
    optional: Optional<A, B>,
    arbA: Arbitrary<A>,
    arbB: Arbitrary<B>,
    EQA: Eq<A>,
    EQ_OPTION_B: Eq<Option<B>>
  ): List<Law> = laws(optional, arbA, arbB, functionAToBArbitrary(arbB), EQA, EQ_OPTION_B)

  /**
   * Warning: Use only when a `Gen.constant()` applies
   */
  fun <A, B> laws(
    optional: Optional<A, B>,
    arbA: Arbitrary<A>,
    arbB: Arbitrary<B>,
    arbBToB: Arbitrary<(B) -> B>,
    EQA: Eq<A>,
    EQ_OPTION_B: Eq<Option<B>>
  ): List<Law> = laws(Arbitrary(Gen.elements(optional)), arbA, arbB, arbBToB, EQA, EQ_OPTION_B)

  fun <A, B> getOptionSet(arbOptional: Arbitrary<Optional<A, B>>, arbA: Arbitrary<A>, EQA: Eq<A>): Property =
    forAll(Tuple2.arbitrary(arbOptional, arbA)) { (optional, a) ->
      optional.run {
        getOrModify(a).fold(::identity) { set(a, it) }
          .eqv(a, EQA)
      }
    }

  fun <A, B> setGetOption(
    arbOptional: Arbitrary<Optional<A, B>>,
    arbA: Arbitrary<A>,
    arbB: Arbitrary<B>,
    EQ_OPTION_B: Eq<Option<B>>
  ): Property =
    forAll(Tuple3.arbitrary(arbOptional, arbA, arbB)) { (optional, a, b) ->
      optional.run {
        getOption(set(a, b))
          .eqv(getOption(a).map { b }, EQ_OPTION_B)
      }
    }

  fun <A, B> setIdempotent(arbOptional: Arbitrary<Optional<A, B>>, arbA: Arbitrary<A>, arbB: Arbitrary<B>, EQA: Eq<A>): Property =
    forAll(Tuple3.arbitrary(arbOptional, arbA, arbB)) { (optional, a, b) ->
      optional.run {
        set(set(a, b), b)
          .eqv(set(a, b), EQA)
      }
    }

  fun <A, B> modifyIdentity(arbOptional: Arbitrary<Optional<A, B>>, arbA: Arbitrary<A>, EQA: Eq<A>): Property =
    forAll(Tuple2.arbitrary(arbOptional, arbA)) { (optional, a) ->
      optional.run {
        modify(a, ::identity)
          .eqv(a, EQA)
      }
    }

  fun <A, B> composeModify(arbOptional: Arbitrary<Optional<A, B>>, arbA: Arbitrary<A>, arbBToB: Arbitrary<(B) -> B>, EQA: Eq<A>): Property =
    forAll(Tuple4.arbitrary(arbOptional, arbA, arbBToB, arbBToB)) { (optional, a, f, g) ->
      optional.run {
        modify(modify(a, f), g)
          .eqv(modify(a, g compose f), EQA)
      }
    }

  fun <A, B> consistentSetModify(arbOptional: Arbitrary<Optional<A, B>>, arbA: Arbitrary<A>, arbB: Arbitrary<B>, EQA: Eq<A>): Property =
    forAll(Tuple3.arbitrary(arbOptional, arbA, arbB)) { (optional, a, b) ->
      optional.run {
        set(a, b)
          .eqv(modify(a) { b }, EQA)
      }
    }

  fun <A, B> consistentModifyModifyId(
    arbOptional: Arbitrary<Optional<A, B>>,
    arbA: Arbitrary<A>,
    arbBToB: Arbitrary<(B) -> B>,
    EQA: Eq<A>
  ): Property =
    forAll(Tuple3.arbitrary(arbOptional, arbA, arbBToB)) { (optional, a, f) ->
      optional.run {
        modify(a, f)
          .eqv(modifyF(Id.applicative(), a) { Id.just(f(it)) }.value(), EQA)
      }
    }

  fun <A, B> consistentGetOptionModifyId(
    arbOptional: Arbitrary<Optional<A, B>>,
    arbA: Arbitrary<A>,
    EQ_OPTION_B: Eq<Option<B>>
  ): Property {
    val firstMonoid = object : Monoid<FirstOption<B>> {
      override fun empty(): FirstOption<B> = FirstOption(None)
      override fun FirstOption<B>.combine(b: FirstOption<B>): FirstOption<B> =
        if (option.fold({ false }, { true })) this else b
    }

    return forAll(Tuple2.arbitrary(arbOptional, arbA)) { (optional, a) ->
      optional.run {
        modifyF(Const.applicative(firstMonoid), a) { b ->
          Const(FirstOption(Some(b)))
        }.value().option.eqv(getOption(a), EQ_OPTION_B)
      }
    }
  }

  @PublishedApi
  internal data class FirstOption<A>(val option: Option<A>)

}