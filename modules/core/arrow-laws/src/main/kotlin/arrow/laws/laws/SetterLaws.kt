package arrow.laws.laws

import arrow.core.Tuple2
import arrow.core.Tuple3
import arrow.core.compose
import arrow.core.identity
import arrow.laws.generators.functionAToBArbitrary
import arrow.optics.Setter
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.propCheck.property.testable.testable
import arrow.typeclasses.Eq

object SetterLaws {

  fun <A, B> laws(setter: Setter<A, B>, arbA: Arbitrary<A>, arbB: Arbitrary<B>, EQA: Eq<A>): List<Law> =
    laws(setter, arbA, arbB, functionAToBArbitrary(arbB), EQA)

  fun <A, B> laws(setter: Setter<A, B>, arbA: Arbitrary<A>, arbB: Arbitrary<B>, arbBToB: Arbitrary<(B) -> B>, EQA: Eq<A>) = listOf(
    Law("Setter law: set is idempotent", setter.setIdempotent(arbA, arbB, EQA)),
    Law("Setter law: modify identity", setter.modifyIdentity(arbA, EQA)),
    Law("Setter law: compose modify", setter.composeModify(arbA, EQA, arbBToB)),
    Law("Setter law: consistent set modify", setter.consistentSetModify(arbA, arbB, EQA))
  )

  fun <A, B> Setter<A, B>.setIdempotent(arbA: Arbitrary<A>, arbB: Arbitrary<B>, EQA: Eq<A>): Property =
    forAll(Tuple2.arbitrary(arbA, arbB)) { (a, b) ->
      set(set(a, b), b).eqv(set(a, b), EQA)
    }

  fun <A, B> Setter<A, B>.modifyIdentity(arbA: Arbitrary<A>, EQA: Eq<A>): Property =
    forAll(arbA.arbitrary(), Property.testable()) { a ->
      modify(a, ::identity).eqv(a, EQA)
    }

  fun <A, B> Setter<A, B>.composeModify(arbA: Arbitrary<A>, EQA: Eq<A>, arbBToB: Arbitrary<(B) -> B>): Property =
    forAll(Tuple3.arbitrary(arbA, arbBToB, arbBToB)) { (a, f, g) ->
      modify(modify(a, f), g).eqv(modify(a, g compose f), EQA)
    }

  fun <A, B> Setter<A, B>.consistentSetModify(arbA: Arbitrary<A>, arbB: Arbitrary<B>, EQA: Eq<A>): Property =
    forAll(Tuple2.arbitrary(arbA, arbB)) { (a, b) ->
      modify(a) { b }.eqv(set(a, b), EQA)
    }
}
