package arrow.test.laws

import arrow.Kind
import arrow.core.Tuple2
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoidal

object MonoidalLaws {

  fun <F, A> laws(
    MDAL: Monoidal<F>,
    arbF: Arbitrary<Kind<F, A>>,
    EQ: Eq<Kind<F, Tuple2<A, A>>>,
    BIJECTION: (Kind<F, Tuple2<Tuple2<A, A>, A>>) -> (Kind<F, Tuple2<A, Tuple2<A, A>>>),
    ASSOCIATIVE_SEMIGROUPAL_EQ: Eq<Kind<F, Tuple2<A, Tuple2<A, A>>>>
  ): List<Law> =
    SemigroupalLaws.laws(MDAL, arbF, BIJECTION, ASSOCIATIVE_SEMIGROUPAL_EQ) + listOf(
      Law("Monoidal Laws: Left identity", MDAL.monoidalLeftIdentity(arbF, EQ)),
      Law("Monoidal Laws: Right identity", MDAL.monoidalRightIdentity(arbF, EQ))
    )

  private fun <F, A> Monoidal<F>.monoidalLeftIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, Tuple2<A, A>>>): Property =
    forAll(arbF) { fa ->
      identity<A>().product(fa).eqv(identity(), EQ)
    }

  private fun <F, A> Monoidal<F>.monoidalRightIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, Tuple2<A, A>>>): Property =
    forAll(arbF) { fa ->
      fa.product(identity<A>()).eqv(identity(), EQ)
    }
}
