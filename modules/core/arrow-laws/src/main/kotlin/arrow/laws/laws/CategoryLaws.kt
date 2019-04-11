package arrow.laws.laws

import arrow.Kind2
import arrow.core.Tuple3
import arrow.propCheck.Property
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.eqv
import arrow.propCheck.forAll
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Category
import arrow.typeclasses.Eq

object CategoryLaws {

  fun <F, A> laws(C: Category<F>, arbF: Arbitrary<Kind2<F, A, A>>, EQ: Eq<Kind2<F, A, A>>) =
    laws(C, arbF, EQ, arbF, EQ)

  fun <F, A, B> laws(C: Category<F>, arbFAA: Arbitrary<Kind2<F, A, A>>, EQAA: Eq<Kind2<F, A, A>>, arbF: Arbitrary<Kind2<F, A, B>>, EQ: Eq<Kind2<F, A, B>>): List<Law> =
    listOf(
      Law("Category Laws: right identity", C.rightIdentity(arbF, EQ)),
      Law("Category Laws: left identity", C.leftIdentity(arbFAA, EQAA)),
      Law("Category Laws: associativity", C.associativity(arbF, EQ))
    )

  fun <F, A, B> Category<F>.rightIdentity(arbF: Arbitrary<Kind2<F, A, B>>, EQ: Eq<Kind2<F, A, B>>): Property =
    forAll(arbF) { fa ->
      fa.compose(id()).eqv(fa, EQ)
    }

  fun <F, A> Category<F>.leftIdentity(arbF: Arbitrary<Kind2<F, A, A>>, EQ: Eq<Kind2<F, A, A>>): Property =
    forAll(arbF) { fa ->
      id<A>().compose(fa).eqv(fa, EQ)
    }

  fun <F, A, B> Category<F>.associativity(arbF: Arbitrary<Kind2<F, A, B>>, EQ: Eq<Kind2<F, A, B>>): Property =
    forAll(Tuple3.arbitrary(
      arbF,
      arbF,
      arbF
    )) { (a, b, c) ->
      a.compose(b).compose(c).eqv(a.compose(b.compose(c)), EQ)
    }
}