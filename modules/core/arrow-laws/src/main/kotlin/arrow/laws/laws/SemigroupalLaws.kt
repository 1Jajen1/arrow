package arrow.laws.laws

import arrow.Kind
import arrow.core.Tuple2
import arrow.core.Tuple3
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.propCheck.mapSize
import arrow.typeclasses.Eq
import arrow.typeclasses.Semigroupal

object SemigroupalLaws {

  fun <F, A> laws(
    SGAL: Semigroupal<F>,
    arbF: Arbitrary<Kind<F, A>>,
    bijection: (Kind<F, Tuple2<Tuple2<A, A>, A>>) -> (Kind<F, Tuple2<A, Tuple2<A, A>>>),
    EQ: Eq<Kind<F, Tuple2<A, Tuple2<A, A>>>>
  ): List<Law> = listOf(
    Law("Semigroupal: Bijective associativity", mapSize(SGAL.semigroupalAssociative(arbF, bijection, EQ)) { it / 5 })
  )

  private fun <F, A> Semigroupal<F>.semigroupalAssociative(
    arbF: Arbitrary<Kind<F, A>>,
    bijection: (Kind<F, Tuple2<Tuple2<A, A>, A>>) -> (Kind<F, Tuple2<A, Tuple2<A, A>>>),
    EQ: Eq<Kind<F, Tuple2<A, Tuple2<A, A>>>>
  ) = forAll(Tuple3.arbitrary(arbF, arbF, arbF)) { (a, b, c) ->
    (a * (b * c))
      .eqv(bijection((a * b) * c), EQ)
  }
}
