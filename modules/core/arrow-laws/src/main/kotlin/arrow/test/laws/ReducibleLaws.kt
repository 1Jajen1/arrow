package arrow.test.laws

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.eq
import arrow.core.extensions.monoid
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid
import arrow.typeclasses.Reducible

object ReducibleLaws {

  fun <F, A> laws(RF: Reducible<F>, arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQ: Eq<A>, EQ_OPTION_A: Eq<Option<A>>): List<Law> =
    FoldableLaws.laws(RF, arbF, arbAToA, MA, EQ) + listOf(
      Law("Reducible Laws: reduceLeftTo consistent with reduceMap", RF.reduceLeftToConsistentWithReduceMap(arbF, arbAToA, MA, EQ)),
      Law("Reducible Laws: reduceRightTo consistent with reduceMap", RF.reduceRightToConsistentWithReduceMap(arbF, arbAToA, MA, EQ)),
      Law("Reducible Laws: reduceRightTo consistent with reduceRightToOption", RF.reduceRightToConsistentWithReduceRightToOption(arbF, arbAToA, MA, EQ_OPTION_A)),
      Law("Reducible Laws: reduceRight consistent with reduceRightOption", RF.reduceRightConsistentWithReduceRightOption(arbF, arbAToA, EQ_OPTION_A)),
      Law("Reducible Laws: reduce reduce left consistent", RF.reduceReduceLeftConsistent(arbF, MA, EQ)),
      Law("Reducible Laws: size consistent", RF.sizeConsistent(arbF))
    )

  fun <F, A> Reducible<F>.reduceLeftToConsistentWithReduceMap(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQ: Eq<A>) =
    forAll(Tuple2.arbitrary(
      arbAToA,
      arbF
    )) { (f, fa) ->
      with(MA) {
        fa.reduceMap(this, f).eqv(fa.reduceLeftTo(f) { b, a -> b.combine(f(a)) }, EQ)
      }
    }

  fun <F, A> Reducible<F>.reduceRightToConsistentWithReduceMap(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQ: Eq<A>) =
    forAll(Tuple2.arbitrary(
      arbAToA,
      arbF
    )) { (f, fa) ->
      with(MA) {
        fa.reduceMap(this, f).eqv(fa.reduceRightTo(f) { a, eb -> eb.map { f(a).combine(it) } }.value(), EQ)
      }
    }

  fun <F, A> Reducible<F>.reduceRightToConsistentWithReduceRightToOption(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQ: Eq<Option<A>>) =
    forAll(Tuple2.arbitrary(
      arbAToA,
      arbF
    )) { (f, fa) ->
      with(MA) {
        fa.reduceRightToOption(f) { a, eb -> eb.map { f(a).combine(it) } }.value()
          .eqv(fa.reduceRightTo(f) { a, eb -> eb.map { f(a).combine(it) } }.map { Option(it) }.value(), EQ)
      }
    }

  fun <F, A> Reducible<F>.reduceRightConsistentWithReduceRightOption(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Option<A>>) =
    forAll(Tuple2.arbitrary(
      Arbitrary(arbAToA.arbitrary().map { it.compose<Tuple2<A, A>, A, A> { it.a } }),
      arbF
    )) { (f, fa) ->
      fa.reduceRight { a1, e2 -> Eval.Now(f(a1 toT e2.value())) }.map { Option(it) }.value()
        .eqv(fa.reduceRightOption { a1, e2 -> Eval.Now(f(a1 toT e2.value())) }.value(), EQ)
    }

  fun <F, A> Reducible<F>.reduceReduceLeftConsistent(arbF: Arbitrary<Kind<F, A>>, MA: Monoid<A>, EQ: Eq<A>) =
    forAll(arbF) { fa ->
      with(MA) {
        fa.reduce(this).eqv(fa.reduceLeft { a1, a2 -> a1.combine(a2) }, EQ)
      }
    }

  fun <F, A> Reducible<F>.sizeConsistent(arbF: Arbitrary<Kind<F, A>>) =
    forAll(arbF) { fa ->
      with(Long.monoid()) {
        fa.size(this).eqv(fa.reduceMap(this) { 1L }, Long.eq())
      }
    }
}
