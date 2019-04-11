package arrow.laws.laws

import arrow.Kind
import arrow.core.Tuple2
import arrow.core.extensions.monoid
import arrow.laws.generators.applicativeArbitrary
import arrow.laws.generators.functionAToBArbitrary
import arrow.mtl.typeclasses.MonadWriter
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Monoid

object MonadWriterLaws {

  fun <F> laws(MW: MonadWriter<F, Int>, EQ: Eq<Kind<F, Int>>, EQ_TUP: Eq<Kind<F, Tuple2<Int, Int>>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> = laws(
    MW, Int.monoid(),
    Int.arbitrary(),
    EQ, EQ_TUP, unsafeRun
  )

  fun <F, W> laws(MW: MonadWriter<F, W>, MOW: Monoid<W>, arbW: Arbitrary<W>, EQ: Eq<Kind<F, Int>>, EQ_TUP: Eq<Kind<F, Tuple2<W, Int>>>, unsafeRun: Kind<F, Int>.() -> Int): List<Law> = laws(
    MW, MOW,
    Int.arbitrary(),
    arbW,
    Int.monoid(),
    EQ, EQ_TUP, unsafeRun
  )

  fun <F, W, A> laws(MW: MonadWriter<F, W>, MOW: Monoid<W>, arbA: Arbitrary<A>, arbW: Arbitrary<W>, MA: Monoid<A>, EQ: Eq<Kind<F, A>>, EQ_TUP: Eq<Kind<F, Tuple2<W, A>>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MW, MOW, arbA, arbW, MA, functionAToBArbitrary(arbA), EQ, EQ_TUP, unsafeRun)

  fun <F, W, A> laws(MW: MonadWriter<F, W>, MOW: Monoid<W>, arbA: Arbitrary<A>, arbW: Arbitrary<W>, MA: Monoid<A>, arbAToA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>, EQ_TUP: Eq<Kind<F, Tuple2<W, A>>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MW, MOW, applicativeArbitrary(arbA, MW), arbA, arbW, MA, arbAToA, arbAToA, EQ, EQ_TUP, unsafeRun)

  fun <F, W, A> laws(MW: MonadWriter<F, W>, MOW: Monoid<W>, arbF: Arbitrary<Kind<F, A>>, arbA: Arbitrary<A>, arbW: Arbitrary<W>, MA: Monoid<A>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>, EQ_TUP: Eq<Kind<F, Tuple2<W, A>>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MW, MOW, arbF, arbA, arbW, MA, arbAtoA, arbAtoA, EQ, EQ_TUP, unsafeRun)

  fun <F, W, A, B> laws(MW: MonadWriter<F, W>, MOW: Monoid<W>, arbA: Arbitrary<A>, arbW: Arbitrary<W>, MA: Monoid<A>, arbAToB: Arbitrary<(A) -> B>, arbBToA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>, EQ_TUP: Eq<Kind<F, Tuple2<W, A>>>, unsafeRun: Kind<F, Int>.() -> Int) =
    laws(MW, MOW, applicativeArbitrary(arbA, MW), arbA, arbW, MA, arbAToB, arbBToA, EQ, EQ_TUP, unsafeRun)

  fun <F, W, A, B> laws(
    MW: MonadWriter<F, W>,
    MOW: Monoid<W>,
    arbF: Arbitrary<Kind<F, A>>,
    arbA: Arbitrary<A>,
    arbW: Arbitrary<W>,
    MA: Monoid<A>,
    arbAToB: Arbitrary<(A) -> B>,
    arbBToA: Arbitrary<(B) -> A>,
    EQ: Eq<Kind<F, A>>,
    EQ_TUP: Eq<Kind<F, Tuple2<W, A>>>,
    unsafeRun: Kind<F, Int>.() -> Int
  ): List<Law> =
    MonadLaws.laws(MW, arbF, arbA, MA, arbAToB, arbBToA, EQ, unsafeRun) + listOf(
      Law("Monad Writer Laws: writer just", MW.monadWriterWriterJust(arbA, MOW, EQ)),
      Law("Monad Writer Laws: tell fusion", MW.monadWriterTellFusion(arbW, MOW)),
      Law("Monad Writer Laws: listen just", MW.monadWriterListenJust(arbA, MOW, EQ_TUP)),
      Law("Monad Writer Laws: listen writer", MW.monadWriterListenWriter(arbA, arbW, EQ_TUP))
    )

  fun <F, W, A> MonadWriter<F, W>.monadWriterWriterJust(
    arbA: Arbitrary<A>,
    MOW: Monoid<W>,
    EQ: Eq<Kind<F, A>>
  ): Property =
    forAll(arbA) { a ->
      writer(Tuple2(MOW.empty(), a)).eqv(just(a), EQ)
    }

  fun <F, W> MonadWriter<F, W>.monadWriterTellFusion(
    arbW: Arbitrary<W>,
    MOW: Monoid<W>
  ): Property =
    forAll(Tuple2.arbitrary(arbW, arbW)) { (x, y) ->
      tell(x).flatMap { tell(y) }.eqv(tell(MOW.run { x.combine(y) }), Eq.any())
    }

  fun <F, W, A> MonadWriter<F, W>.monadWriterListenJust(
    arbA: Arbitrary<A>,
    MOW: Monoid<W>,
    EqTupleWA: Eq<Kind<F, Tuple2<W, A>>>
  ): Property =
    forAll(arbA) { a ->
      just(a).listen().eqv(just(Tuple2(MOW.empty(), a)), EqTupleWA)
    }

  fun <F, W, A> MonadWriter<F, W>.monadWriterListenWriter(
    arbA: Arbitrary<A>,
    arbW: Arbitrary<W>,
    EQ_TUP: Eq<Kind<F, Tuple2<W, A>>>
  ): Property =
    forAll(Tuple2.arbitrary(arbW, arbA)) { tupleWA ->
      writer(tupleWA).listen().eqv(tell(tupleWA.a).map { tupleWA }, EQ_TUP)
    }
}
