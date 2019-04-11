package arrow.laws.laws

import arrow.Kind
import arrow.data.extensions.list.foldable.fold
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.typeclasses.Eq
import arrow.typeclasses.MonoidK

object MonoidKLaws {

  fun <F, A> laws(SGK: MonoidK<F>, arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): List<Law> =
    SemigroupKLaws.laws(SGK, arbF, EQ) + listOf(
      Law("MonoidK Laws: Left identity", SGK.monoidKLeftIdentity(arbF, EQ)),
      Law("MonoidK Laws: Right identity", SGK.monoidKRightIdentity(arbF, EQ)),
      Law("MonoidK Laws: Fold with Monoid instance", SGK.monoidKFold(arbF, EQ))
    )

  fun <F, A> MonoidK<F>.monoidKLeftIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      empty<A>().combineK(fa).eqv(fa, EQ)
    }

  fun <F, A> MonoidK<F>.monoidKRightIdentity(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property =
    forAll(arbF) { fa ->
      fa.combineK(empty()).eqv(fa, EQ)
    }

  fun <F, A> MonoidK<F>.monoidKFold(arbF: Arbitrary<Kind<F, A>>, EQ: Eq<Kind<F, A>>): Property {
    val mo = this
    return forAll(arbF) { fa ->
      listOf(fa).fold(mo.algebra()).eqv(fa, EQ)
    }
  }
}
