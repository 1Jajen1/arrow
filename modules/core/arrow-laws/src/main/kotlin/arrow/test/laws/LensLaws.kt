package arrow.test.laws

import arrow.core.*
import arrow.core.extensions.const.applicative.applicative
import arrow.core.extensions.id.functor.functor
import arrow.optics.Lens
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

object LensLaws {

  fun <A, B> laws(
    arbLens: Arbitrary<Lens<A, B>>,
    arbA: Arbitrary<A>,
    arbB: Arbitrary<B>,
    arbBToB: Arbitrary<(B) -> B>,
    EQA: Eq<A>,
    EQB: Eq<B>,
    MB: Monoid<B>
  ): List<Law> =
    listOf(
      Law("Lens law: get set", lensGetSet(arbLens, arbA, EQA)),
      Law("Lens law: set get", lensSetGet(arbLens, arbA, arbB, EQB)),
      Law("Lens law: is set idempotent", lensSetIdempotent(arbLens, arbA, arbB, EQA)),
      Law("Lens law: modify identity", lensModifyIdentity(arbLens, arbA, EQA)),
      Law("Lens law: compose modify", lensComposeModify(arbLens, arbA, arbBToB, EQA)),
      Law("Lens law: consistent set modify", lensConsistentSetModify(arbLens, arbA, arbB, EQA)),
      Law("Lens law: consistent modify modify id", lensConsistentModifyModifyId(arbLens, arbA, arbBToB, EQA)),
      Law("Lens law: consistent get modify id", lensConsistentGetModifyid(arbLens, arbA, EQB, MB))
    )

  /**
   * Warning: Use only when a `Gen.constant()` applies
   */
  fun <A, B> laws(
    lens: Lens<A, B>,
    arbA: Arbitrary<A>,
    arbB: Arbitrary<B>,
    arbBToB: Arbitrary<(B) -> B>,
    EQA: Eq<A>,
    EQB: Eq<B>,
    MB: Monoid<B>
  ): List<Law> = laws(Arbitrary.invoke(Gen.elements(lens)), arbA, arbB, arbBToB, EQA, EQB, MB)

  fun <A, B> lensGetSet(arbLens: Arbitrary<Lens<A, B>>, arbA: Arbitrary<A>, EQA: Eq<A>): Property =
    forAll(Tuple2.arbitrary(arbLens, arbA)) { (lens, a) ->
      lens.run {
        set(a, get(a)).eqv(a, EQA)
      }
    }

  fun <A, B> lensSetGet(arbLens: Arbitrary<Lens<A, B>>, arbA: Arbitrary<A>, arbB: Arbitrary<B>, EQB: Eq<B>): Property =
    forAll(Tuple3.arbitrary(arbLens, arbA, arbB)) { (lens, a, b) ->
      lens.run {
        get(set(a, b)).eqv(b, EQB)
      }
    }

  fun <A, B> lensSetIdempotent(arbLens: Arbitrary<Lens<A, B>>, arbA: Arbitrary<A>, arbB: Arbitrary<B>, EQA: Eq<A>): Property =
    forAll(Tuple3.arbitrary(arbLens, arbA, arbB)) { (lens, a, b) ->
      lens.run {
        set(set(a, b), b).eqv(set(a, b), EQA)
      }
    }

  fun <A, B> lensModifyIdentity(arbLens: Arbitrary<Lens<A, B>>, arbA: Arbitrary<A>, EQA: Eq<A>): Property =
    forAll(Tuple2.arbitrary(arbLens, arbA)) { (lens, a) ->
      lens.run {
        modify(a, ::identity).eqv(a, EQA)
      }
    }

  fun <A, B> lensComposeModify(arbLens: Arbitrary<Lens<A, B>>, arbA: Arbitrary<A>, arbBToB: Arbitrary<(B) -> B>, EQA: Eq<A>): Property =
    forAll(Tuple4.arbitrary(arbLens, arbA, arbBToB, arbBToB)) { (lens, a, f, g) ->
      lens.run {
        modify(modify(a, f), g).eqv(modify(a, g compose f), EQA)
      }
    }

  fun <A, B> lensConsistentSetModify(arbLens: Arbitrary<Lens<A, B>>, arbA: Arbitrary<A>, arbB: Arbitrary<B>, EQA: Eq<A>): Property =
    forAll(Tuple3.arbitrary(arbLens, arbA, arbB)) { (lens, a, b) ->
      lens.run {
        set(a, b).eqv(modify(a) { b }, EQA)
      }
    }

  fun <A, B> lensConsistentModifyModifyId(arbLens: Arbitrary<Lens<A, B>>, arbA: Arbitrary<A>, arbBToB: Arbitrary<(B) -> B>, EQA: Eq<A>): Property =
    forAll(Tuple3.arbitrary(arbLens, arbA, arbBToB)) { (lens, a, f) ->
      lens.run {
        modify(a, f)
          .eqv(modifyF(Id.functor(), a) { Id.just(f(it)) }.value(), EQA)
      }
    }

  fun <A, B> lensConsistentGetModifyid(arbLens: Arbitrary<Lens<A, B>>, arbA: Arbitrary<A>, EQB: Eq<B>, MA: Monoid<B>): Property =
    forAll(Tuple2.arbitrary(arbLens, arbA)) { (lens, a) ->
      lens.run {
        get(a)
          .eqv(modifyF(Const.applicative(MA), a, ::Const).value(), EQB)
      }
    }

}