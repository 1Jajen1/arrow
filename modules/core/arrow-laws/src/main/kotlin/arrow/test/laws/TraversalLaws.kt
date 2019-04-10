package arrow.test.laws

import arrow.core.*
import arrow.data.ListK
import arrow.optics.Traversal
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.propCheck.property.testable.testable
import arrow.typeclasses.Eq

object TraversalLaws {

  fun <A, B : Any> laws(traversal: Traversal<A, B>, arbA: Arbitrary<A>, arbB: Arbitrary<B>, arbBToB: Arbitrary<(B) -> B>, EQA: Eq<A>, EQ_OPTION_B: Eq<Option<B>>, EQ_LIST_B: Eq<ListK<B>>) = listOf(
    Law("Traversal law: head option", traversal.headOption(arbA, EQ_OPTION_B)),
    Law("Traversal law: modify get all", traversal.modifyGetAll(arbA, arbBToB, EQ_LIST_B)),
    Law("Traversal law: set is idempotent", traversal.setIdempotent(arbA, arbB, EQA)),
    Law("Traversal law: modify identity", traversal.modifyIdentity(arbA, EQA)),
    Law("Traversal law: compose modify", traversal.composeModify(arbA, arbBToB, EQA))
  )

  fun <A, B : Any> Traversal<A, B>.headOption(arbA: Arbitrary<A>, EQ_OPTION_B: Eq<Option<B>>): Property =
    forAll(arbA.arbitrary(), Property.testable()) { a ->
      headOption(a)
        .eqv(getAll(a).firstOrNull().toOption(), EQ_OPTION_B)
    }

  fun <A, B> Traversal<A, B>.modifyGetAll(arbA: Arbitrary<A>, arbBToB: Arbitrary<(B) -> B>, EQ_LIST_B: Eq<ListK<B>>): Property =
    forAll(Tuple2.arbitrary(arbA, arbBToB)) { (a, f) ->
      getAll(modify(a, f))
        .eqv(getAll(a).map(f), EQ_LIST_B)
    }

  fun <A, B> Traversal<A, B>.setIdempotent(arbA: Arbitrary<A>, arbB: Arbitrary<B>, EQA: Eq<A>): Property =
    forAll(Tuple2.arbitrary(arbA, arbB)) { (a, b) ->
      set(set(a, b), b)
        .eqv(set(a, b), EQA)
    }

  fun <A, B> Traversal<A, B>.modifyIdentity(arbA: Arbitrary<A>, EQA: Eq<A>): Property =
    forAll(arbA.arbitrary(), Property.testable()) { a ->
      modify(a, ::identity).eqv(a, EQA)
    }

  fun <A, B> Traversal<A, B>.composeModify(arbA: Arbitrary<A>, arbBToB: Arbitrary<(B) -> B>, EQA: Eq<A>): Property =
    forAll(Tuple3.arbitrary(arbA, arbBToB, arbBToB)) { (a, f, g) ->
      modify(modify(a, f), g)
        .eqv(modify(a, g compose f), EQA)
    }
}
