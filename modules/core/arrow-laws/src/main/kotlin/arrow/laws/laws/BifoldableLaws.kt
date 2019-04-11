package arrow.laws.laws

import arrow.Kind2
import arrow.core.Eval
import arrow.core.Tuple3
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Bifoldable
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid

object BifoldableLaws {

  fun <F, A> laws(BF: Bifoldable<F>, arbF: Arbitrary<Kind2<F, A, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQ: Eq<A>): List<Law> =
    listOf(
      Law("Bifoldable Laws: Left bifold consistent with BifoldMap", BF.bifoldLeftConsistentWithBifoldMap(arbF, arbAToA, MA, EQ)),
      Law("Bifoldable Laws: Right bifold consistent with BifoldMap", BF.bifoldRightConsistentWithBifoldMap(arbF, arbAToA, MA, EQ))
    )

  fun <F, A> Bifoldable<F>.bifoldLeftConsistentWithBifoldMap(arbF: Arbitrary<Kind2<F, A, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQ: Eq<A>): Property =
    forAll(Tuple3.arbitrary(
      arbAToA,
      arbAToA,
      arbF
    )) { (f, g, fab) ->
      with(MA) {
        val expected = fab.bifoldLeft(empty(), { c: A, a: A -> c.combine(f(a)) },
          { c: A, b: A -> c.combine(g(b)) })
        expected.eqv(fab.bifoldMap(this, f, g), EQ)
      }
    }

  fun <F, A> Bifoldable<F>.bifoldRightConsistentWithBifoldMap(arbF: Arbitrary<Kind2<F, A, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQ: Eq<A>): Property =
    forAll(Tuple3.arbitrary(
      arbAToA,
      arbAToA,
      arbF
    )) { (f, g, fab) ->
      with(MA) {
        val expected = fab.bifoldRight(Eval.Later { empty() }, { a: A, ec: Eval<A> -> ec.map { c -> f(a).combine(c) } },
          { b: A, ec: Eval<A> -> ec.map { c -> g(b).combine(c) } })
        expected.value().eqv(fab.bifoldMap(this, f, g), EQ)
      }
    }
}
