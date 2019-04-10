package arrow.test.laws

import arrow.Kind
import arrow.core.Eval
import arrow.core.Id
import arrow.core.Tuple2
import arrow.core.extensions.eq
import arrow.core.extensions.id.comonad.extract
import arrow.core.extensions.id.monad.monad
import arrow.core.extensions.monoid
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.testable
import arrow.test.concurrency.SideEffect
import arrow.test.generators.functionAToBArbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Foldable
import arrow.typeclasses.Monoid

object FoldableLaws {

  fun <F, A> laws(FF: Foldable<F>, arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQ: Eq<A>): List<Law> =
    listOf(
      Law("Foldable Laws: Left fold consistent with foldMap", FF.leftFoldConsistentWithFoldMap(arbF, arbAToA, MA, EQ)),
      Law("Foldable Laws: Right fold consistent with foldMap", FF.rightFoldConsistentWithFoldMap(arbF, arbAToA, MA, EQ)),
      Law("Foldable Laws: Exists is consistent with find", FF.existsConsistentWithFind(arbF)),
      Law("Foldable Laws: Exists is lazy", FF.existsIsLazy(arbF)),
      Law("Foldable Laws: ForAll is lazy", FF.forAllIsLazy(arbF)),
      Law("Foldable Laws: ForAll consistent with exists", FF.forallConsistentWithExists(arbF)),
      Law("Foldable Laws: ForAll returns true if isEmpty", FF.forallReturnsTrueIfEmpty(arbF)),
      Law("Foldable Laws: FoldM for Id is equivalent to fold left", FF.foldMIdIsFoldL(arbF, arbAToA, MA, EQ))
    )

  fun <F, A> Foldable<F>.leftFoldConsistentWithFoldMap(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQ: Eq<A>): Property =
    forAll(Tuple2.arbitrary(
      arbAToA,
      arbF
    )) { (f, fa) ->
      with(MA) {
        fa.foldMap(this, f).eqv(fa.foldLeft(empty()) { acc, a -> acc.combine(f(a)) }, EQ)
      }
    }

  fun <F, A> Foldable<F>.rightFoldConsistentWithFoldMap(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQ: Eq<A>): Property =
    forAll(Tuple2.arbitrary(
      arbAToA,
      arbF
    )) { (f, fa) ->
      with(MA) {
        fa.foldMap(this, f).eqv(fa.foldRight(Eval.later { empty() }) { a, lb: Eval<A> -> lb.map { f(a).combine(it) } }.value(), EQ)
      }
    }

  fun <F, A> Foldable<F>.existsConsistentWithFind(arbF: Arbitrary<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<A, Boolean>(Boolean.arbitrary()),
      arbF
    )) { (f, fa) ->
      fa.exists(f).eqv(fa.find(f).fold({ false }, { true }), Eq.any())
    }

  fun <F, A> Foldable<F>.existsIsLazy(arbF: Arbitrary<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      val sideEffect = SideEffect()
      fa.exists {
        sideEffect.increment()
        true
      }
      val expected = if (fa.isEmpty()) 0 else 1

      sideEffect.counter.eqv(expected)
    }

  fun <F, A> Foldable<F>.forAllIsLazy(arbF: Arbitrary<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      val sideEffect = SideEffect()
      fa.forAll {
        sideEffect.increment()
        true
      }
      val expected = if (fa.isEmpty()) 0 else fa.size(Long.monoid())
      sideEffect.counter.toLong().eqv(expected, Long.eq())
    }

  fun <F, A> Foldable<F>.forallConsistentWithExists(arbF: Arbitrary<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<A, Boolean>(Boolean.arbitrary()),
      arbF
    ), Boolean.testable()) { (f, fa) ->
      if (fa.forAll(f)) {
        // if f is true for all elements, then there cannot be an element for which
        // it does not hold.
        val negationExists = fa.exists { a -> !(f(a)) }
        // if f is true for all elements, then either there must be no elements
        // or there must exist an element for which it is true.
        !negationExists && (fa.isEmpty() || fa.exists(f))
      } else true
    }

  fun <F, A> Foldable<F>.forallReturnsTrueIfEmpty(arbF: Arbitrary<Kind<F, A>>): Property =
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<A, Boolean>(Boolean.arbitrary()),
      arbF
    ), Boolean.testable()) { (f, fa) ->
      !fa.isEmpty() || fa.forAll(f)
    }

  fun <F, A> Foldable<F>.foldMIdIsFoldL(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQ: Eq<A>): Property =
    forAll(Tuple2.arbitrary(
      arbAToA,
      arbF
    )) { (f, fa) ->
      with(MA) {
        val foldL: A = fa.foldLeft(empty()) { acc, a -> acc.combine(f(a)) }
        val foldM: A = fa.foldM(Id.monad(), empty()) { acc, a -> Id(acc.combine(f(a))) }.extract()
        foldM.eqv(foldL, EQ)
      }
    }
}
