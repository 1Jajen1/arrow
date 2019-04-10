package arrow.test.laws

import arrow.core.Tuple2
import arrow.core.Tuple3
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid
import arrow.typeclasses.Semiring

object SemiringLaws {

  fun <A> laws(SG: Semiring<A>, arbA: Arbitrary<A>, EQ: Eq<A>): List<Law> =
    MonoidLaws.laws(SG, arbA, EQ) + MonoidLaws.laws(object : Monoid<A> {
      override fun empty(): A = SG.one()
      override fun A.combine(b: A): A = SG.run { combineMultiplicate(b) }
    }, arbA, EQ) + listOf(
      Law("Semiring: Additive commutativity", SG.semiringAdditiveCommutativity(arbA, EQ)),
      Law("Semiring: Multiplicative commutativity", SG.semiringMultiplicativeCommutativity(arbA, EQ)),
      Law("Semiring: Right distributivity", SG.semiringRightDistributivity(arbA, EQ)),
      Law("Semiring: Left distributivity", SG.semiringLeftDistributivity(arbA, EQ)),
      Law("Semiring: Multiplicative left absorption", SG.semiringMultiplicativeLeftAbsorption(arbA, EQ)),
      Law("Semiring: Multiplicative right absorption", SG.semiringMultiplicativeRightAbsorption(arbA, EQ))
    )

  fun <A> Semiring<A>.semiringAdditiveCommutativity(arbA: Arbitrary<A>, EQ: Eq<A>) =
    forAll(Tuple2.arbitrary(arbA, arbA)) { (a, b) ->
      (a + b).eqv((b + a), EQ)
    }

  fun <A> Semiring<A>.semiringMultiplicativeCommutativity(arbA: Arbitrary<A>, EQ: Eq<A>) =
    forAll(Tuple2.arbitrary(arbA, arbA)) { (a, b) ->
      (a * b).eqv((b * a), EQ)
    }

  fun <A> Semiring<A>.semiringRightDistributivity(arbA: Arbitrary<A>, EQ: Eq<A>) =
    forAll(Tuple3.arbitrary(arbA, arbA, arbA)) { (a, b, c) ->
      ((a + b) * (c))
        .eqv(((a * c) + (b * c)), EQ)
    }

  fun <A> Semiring<A>.semiringLeftDistributivity(arbA: Arbitrary<A>, EQ: Eq<A>) =
    forAll(Tuple3.arbitrary(arbA, arbA, arbA)) { (a, b, c) ->
      (a * (b + c))
        .eqv(((a * b) + (a * c)), EQ)
    }

  fun <A> Semiring<A>.semiringMultiplicativeLeftIdentity(arbA: Arbitrary<A>, EQ: Eq<A>) =
    forAll(arbA) { a ->
      (one() * a).eqv(a, EQ)
    }

  fun <A> Semiring<A>.semiringMultiplicativeRightIdentity(arbA: Arbitrary<A>, EQ: Eq<A>) =
    forAll(arbA) { a ->
      (a * one()).eqv(a, EQ)
    }

  fun <A> Semiring<A>.semiringMultiplicativeLeftAbsorption(arbA: Arbitrary<A>, EQ: Eq<A>) =
    forAll(arbA) { a ->
      (zero() * a).eqv(zero(), EQ)
    }

  fun <A> Semiring<A>.semiringMultiplicativeRightAbsorption(arbA: Arbitrary<A>, EQ: Eq<A>) =
    forAll(arbA) { a ->
      (a * zero()).eqv(zero(), EQ)
    }
}
