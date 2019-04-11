package arrow.laws.laws

import arrow.Kind
import arrow.core.Tuple5
import arrow.core.compose
import arrow.core.identity
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple5.arbitrary.arbitrary
import arrow.typeclasses.Eq
import arrow.typeclasses.Invariant

object InvariantLaws {

  fun <F, A> laws(IF: Invariant<F>, arbF: Arbitrary<Kind<F, A>>, arbAtoA: Arbitrary<(A) -> A>, EQ: Eq<Kind<F, A>>) =
    laws(IF, arbF, arbAtoA, arbAtoA, EQ)

  fun <F, A, B> laws(IF: Invariant<F>, arbF: Arbitrary<Kind<F, A>>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>): List<Law> =
    listOf(
      Law("Invariant Laws: Invariant Identity", IF.identity(arbF, EQ)),
      Law("Invariant Laws: Invariant Composition", IF.composition(arbF, arbAtoB, arbBtoA, EQ))
    )

  fun <F, A> Invariant<F>.identity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      val imap: Kind<F, A> = fa.imap<A, A>(::identity, ::identity)
      imap.eqv(fa, EQ)
    }

  fun <F, A, B> Invariant<F>.composition(arbF: Arbitrary<Kind<F, A>>, arbAtoB: Arbitrary<(A) -> B>, arbBtoA: Arbitrary<(B) -> A>, EQ: Eq<Kind<F, A>>): Property =
    forAll(
      Tuple5.arbitrary(
        arbF,
        arbAtoB,
        arbBtoA,
        arbBtoA,
        arbAtoB
      )
    ) { (fa, f1, f2, g1, g2) ->
      fa.imap(f1, f2).imap(g1, g2).eqv(fa.imap(g1 compose f1, f2 compose g2), EQ)
    }
}