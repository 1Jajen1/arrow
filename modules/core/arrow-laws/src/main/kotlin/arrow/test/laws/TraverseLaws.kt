package arrow.test.laws

import arrow.Kind
import arrow.core.*
import arrow.core.extensions.const.applicative.applicative
import arrow.core.extensions.id.applicative.applicative
import arrow.core.extensions.id.comonad.extract
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.id.arbitrary.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.test.generators.functionAToBArbitrary
import arrow.test.generators.makeFunAtoA
import arrow.typeclasses.*

typealias TI<A> = Tuple2<IdOf<A>, IdOf<A>>

typealias TIK<A> = Kind<TIF, A>

@Suppress("UNCHECKED_CAST")
fun <A> TIK<A>.fix(): TIC<A> =
  this as TIC<A>

data class TIC<out A>(val ti: TI<A>) : TIK<A>

class TIF {
  private constructor()
}

object TraverseLaws {
  // FIXME(paco): this implementation will crash the inliner. Wait for fix: https://youtrack.jetbrains.com/issue/KT-18660
  /*
  inline fun <F, A> laws(TF: Traverse<F>, AF: Applicative<F>, EQ: Eq<Kind<F, A>>): List<Law> =
      FoldableLaws.laws(TF, { AF.just(it) }, Eq.any()) + FunctorLaws.laws(AF, EQ) + listOf(
              Law("Traverse Laws: Identity", { identityTraverse(TF, AF, { AF.just(it) }, EQ) }),
              Law("Traverse Laws: Sequential composition", { sequentialComposition(TF, { AF.just(it) }, EQ) }),
              Law("Traverse Laws: Parallel composition", { parallelComposition(TF, { AF.just(it) }, EQ) }),
              Law("Traverse Laws: FoldMap derived", { foldMapDerived(TF, { AF.just(it) }) })
      )
  */


  fun <F, A> laws(TF: Traverse<F>, FF: Functor<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>, EQA: Eq<A>, EQ: Eq<Kind<F, A>>) =
    laws(TF, FF, arbF, arbA, arbAToA, arbAToA, MA, EQA, EQ)

  fun <F, A, B> laws(TF: Traverse<F>, FF: Functor<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, MA: Monoid<A>, EQA: Eq<A>, EQ: Eq<Kind<F, A>>): List<Law> =
    FoldableLaws.laws(TF, arbF, makeFunAtoA(arbAToB, arbBToA), MA, EQA) + FunctorLaws.laws(FF, arbF, arbAToB, arbBToA, EQ) + listOf(
      Law("Traverse Laws: Identity", TF.identityTraverse(FF, arbF, arbA, EQ)),
      Law("Traverse Laws: Sequential composition", TF.sequentialComposition(arbF, arbA, EQ)),
      Law("Traverse Laws: Parallel composition", TF.parallelComposition(arbF, arbA, EQ)),
      Law("Traverse Laws: FoldMap derived", TF.foldMapDerived(arbF, makeFunAtoA(arbAToB, arbBToA), MA))
    )

  fun <F, A> Traverse<F>.identityTraverse(FF: Functor<F>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>) = Id.applicative().run {
    val idApp = this
    forAll(Tuple2.arbitrary(
      functionAToBArbitrary<A, Id<A>>(Id.arbitrary(arbA)),
      arbF
    )) { (f, fa) ->
      fa.traverse(idApp, f).extract()
        .eqv(FF.run { fa.map(f).map { it.extract() } }, EQ)
    }
  }

  fun <F, A> Traverse<F>.sequentialComposition(arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>) = Id.applicative().run {
    val idApp = this
    forAll(
      Tuple3.arbitrary(
        functionAToBArbitrary<A, Id<A>>(Id.arbitrary(arbA)),
        functionAToBArbitrary<A, Id<A>>(Id.arbitrary(arbA)),
        arbF
      )
    ) { (f, g, fha) ->
      val fa = fha.traverse(idApp, f).fix()
      val composed = fa.map { it.traverse(idApp, g) }.value().value()
      val expected = fha.traverse(ComposedApplicative(idApp, idApp)) { a: A -> f(a).map(g).nest() }.unnest().extract().extract()
      composed.eqv(expected, EQ)
    }
  }

  fun <F, A> Traverse<F>.parallelComposition(arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, EQ: Eq<Kind<F, A>>) =
    forAll(
      Tuple3.arbitrary(
        functionAToBArbitrary<A, Id<A>>(Id.arbitrary(arbA)),
        functionAToBArbitrary<A, Id<A>>(Id.arbitrary(arbA)),
        arbF
      )) { (f, g, fha) ->
      val TIA = object : Applicative<TIF> {
        override fun <A> just(a: A): Kind<TIF, A> =
          TIC(Id(a) toT Id(a))

        override fun <A, B> Kind<TIF, A>.ap(ff: Kind<TIF, (A) -> B>): Kind<TIF, B> {
          val (fam, fan) = fix().ti
          val (fm, fn) = ff.fix().ti
          return TIC(Id.applicative().run { fam.ap(fm) toT fan.ap(fn) })
        }

      }

      val TIEQ: Eq<TI<Kind<F, A>>> = Eq { a, b ->
        with(EQ) {
          a.a.extract().eqv(b.a.extract()) && a.b.extract().eqv(b.b.extract())
        }
      }

      val seen: TI<Kind<F, A>> = fha.traverse(TIA) { TIC(f(it) toT g(it)) }.fix().ti
      val expected: TI<Kind<F, A>> = TIC(fha.traverse(Id.applicative(), f) toT fha.traverse(Id.applicative(), g)).ti

      seen.eqv(expected, TIEQ)
    }

  fun <F, A> Traverse<F>.foldMapDerived(arbF: Arbitrary<Kind<F, A>>, arbAToA: Arbitrary<(A) -> A>, MA: Monoid<A>) =
    forAll(Tuple2.arbitrary(
      arbAToA,
      arbF
    )) { (f, fa) ->
      val traversed = fa.traverse(Const.applicative(MA)) { a -> f(a).const() }.value()
      val mapped = fa.foldMap(MA, f)
      mapped.eqv(traversed, Eq.any())
    }
}
