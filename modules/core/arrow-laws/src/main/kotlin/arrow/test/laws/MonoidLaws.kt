package arrow.test.laws

import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid

object MonoidLaws {

  fun <A> laws(M: Monoid<A>, arbA: Arbitrary<A>, EQ: Eq<A>): List<Law> =
    SemigroupLaws.laws(M, arbA, EQ) + listOf(
      Law("Monoid Laws: Left identity", M.monoidLeftIdentity(arbA, EQ)),
      Law("Monoid Laws: Right identity", M.monoidRightIdentity(arbA, EQ))
    )

  fun <A> Monoid<A>.monoidLeftIdentity(arbA: Arbitrary<A>, EQ: Eq<A>): Property =
    forAll(arbA) { a ->
      (empty().combine(a)).eqv(a, EQ)
    }

  fun <A> Monoid<A>.monoidRightIdentity(arbA: Arbitrary<A>, EQ: Eq<A>): Property =
    forAll(arbA) { a ->
      a.combine(empty()).eqv(a, EQ)
    }

}
