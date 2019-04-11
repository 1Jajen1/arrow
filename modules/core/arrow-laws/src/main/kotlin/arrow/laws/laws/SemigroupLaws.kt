package arrow.laws.laws

import arrow.core.Tuple3
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Semigroup

object SemigroupLaws {

  fun <A> laws(SG: Semigroup<A>, arbA: Arbitrary<A>, EQ: Eq<A>): List<Law> =
    listOf(Law("Semigroup: associativity", SG.semigroupAssociative(arbA, EQ)))

  fun <A> Semigroup<A>.semigroupAssociative(arbA: Arbitrary<A>, EQ: Eq<A>): Property =
    forAll(Tuple3.arbitrary(arbA, arbA, arbA)) { (a, b, c) ->
      (a + (b + c)).eqv(((a + b) + c), EQ)
    }

}
