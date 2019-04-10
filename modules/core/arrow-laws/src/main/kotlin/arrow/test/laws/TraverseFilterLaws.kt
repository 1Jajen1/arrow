package arrow.test.laws

import arrow.Kind
import arrow.core.None
import arrow.core.Some
import arrow.core.Tuple2
import arrow.core.extensions.eq
import arrow.core.extensions.monoid
import arrow.mtl.typeclasses.TraverseFilter
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.test.generators.applicativeArbitrary
import arrow.test.generators.functionAToBArbitrary
import arrow.typeclasses.Applicative
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid

object TraverseFilterLaws {

  fun <F> laws(TF: TraverseFilter<F>, AP: Applicative<F>, EQ: Eq<Kind<F, Int>>, EQ_NESTED: Eq<Kind<F, Kind<F, Int>>>): List<Law> = laws(
    TF, AP, Int.arbitrary(), functionAToBArbitrary(Int.arbitrary()), Int.monoid(), Int.eq(), EQ, EQ_NESTED
  )

  fun <F, A> laws(TF: TraverseFilter<F>, AP: Applicative<F>, arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQA: Eq<A>, EQ: Eq<Kind<F, A>>, EQ_NESTED: Eq<Kind<F, Kind<F, A>>>): List<Law> = laws(
    TF, AP,
    applicativeArbitrary(arbA, AP),
    arbA, arbAToA, MA, EQA, EQ, EQ_NESTED
  )

  fun <F, A> laws(TF: TraverseFilter<F>, AP: Applicative<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQA: Eq<A>, EQ: Eq<Kind<F, A>>, EQ_NESTED: Eq<Kind<F, Kind<F, A>>>) =
    laws(TF, AP, arbF, arbA, arbAToA, arbAToA, MA, EQA, EQ, EQ_NESTED)

  //FIXME(paco): TraverseLaws cannot receive AP::just due to a crash caused by the inliner. Check in TraverseLaws why.
  fun <F, A, B> laws(TF: TraverseFilter<F>, AP: Applicative<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, MA: Monoid<A>, EQA: Eq<A>, EQ: Eq<Kind<F, A>>, EQ_NESTED: Eq<Kind<F, Kind<F, A>>>): List<Law> =
    TraverseLaws.laws(TF, AP, arbF, arbA, arbAToB, arbBToA, MA, EQA, EQ) + listOf(
      Law("TraverseFilter Laws: Identity", TF.identityTraverseFilter(AP, arbF, EQ_NESTED)),
      Law("TraverseFilter Laws: filterA consistent with TraverseFilter", TF.filterAconsistentWithTraverseFilter(AP, arbF, EQ_NESTED))
    )

  fun <F, A> TraverseFilter<F>.identityTraverseFilter(AP: Applicative<F>, arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, Kind<F, A>>>) =
    forAll(arbF) { fa ->
      fa.traverseFilter(AP) { AP.just(Some(it)) }.eqv(AP.just(fa), EQ)
    }

  fun <F, A> TraverseFilter<F>.filterAconsistentWithTraverseFilter(AP: Applicative<F>, arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, Kind<F, A>>>) = run {
    forAll(
      Tuple2.arbitrary(
        arbF,
        functionAToBArbitrary<A, Kind<F, Boolean>>(applicativeArbitrary(Boolean.arbitrary(), AP))
      )) { (fa, f) ->
      fa.filterA(f, AP).eqv(fa.traverseFilter(AP) { a -> f(a).map { b: Boolean -> if (b) Some(a) else None } }, EQ)
    }
  }
}
