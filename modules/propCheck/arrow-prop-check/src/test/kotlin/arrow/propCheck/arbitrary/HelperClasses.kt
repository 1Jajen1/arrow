package arrow.propCheck.arbitrary

import arrow.Kind
import arrow.core.Tuple2
import arrow.core.extensions.eq
import arrow.core.extensions.order
import arrow.core.extensions.show
import arrow.core.toT
import arrow.data.ListK
import arrow.data.extensions.listk.eq.eq
import arrow.data.k
import arrow.propCheck.*
import arrow.propCheck.arbitrary.blind.arbitrary.arbitrary
import arrow.propCheck.arbitrary.blind.eq.eq
import arrow.propCheck.arbitrary.blind.functor.functor
import arrow.propCheck.arbitrary.blind.show.show
import arrow.propCheck.arbitrary.fixed.arbitrary.arbitrary
import arrow.propCheck.arbitrary.fixed.eq.eq
import arrow.propCheck.arbitrary.fixed.functor.functor
import arrow.propCheck.arbitrary.fixed.show.show
import arrow.propCheck.arbitrary.negative.arbitrary.arbitrary
import arrow.propCheck.arbitrary.negative.eq.eq
import arrow.propCheck.arbitrary.negative.functor.functor
import arrow.propCheck.arbitrary.negative.show.show
import arrow.propCheck.arbitrary.nonnegative.arbitrary.arbitrary
import arrow.propCheck.arbitrary.nonnegative.eq.eq
import arrow.propCheck.arbitrary.nonnegative.functor.functor
import arrow.propCheck.arbitrary.nonnegative.show.show
import arrow.propCheck.arbitrary.nonpositive.arbitrary.arbitrary
import arrow.propCheck.arbitrary.nonpositive.eq.eq
import arrow.propCheck.arbitrary.nonpositive.functor.functor
import arrow.propCheck.arbitrary.nonpositive.show.show
import arrow.propCheck.arbitrary.orderedlist.arbitrary.arbitrary
import arrow.propCheck.arbitrary.orderedlist.eq.eq
import arrow.propCheck.arbitrary.orderedlist.functor.functor
import arrow.propCheck.arbitrary.orderedlist.show.show
import arrow.propCheck.arbitrary.positive.arbitrary.arbitrary
import arrow.propCheck.arbitrary.positive.eq.eq
import arrow.propCheck.arbitrary.positive.functor.functor
import arrow.propCheck.arbitrary.positive.show.show
import arrow.propCheck.arbitrary.shrink2.arbitrary.arbitrary
import arrow.propCheck.arbitrary.shrink2.eq.eq
import arrow.propCheck.arbitrary.shrink2.functor.functor
import arrow.propCheck.arbitrary.shrink2.show.show
import arrow.propCheck.arbitrary.shrinking.arbitrary.arbitrary
import arrow.propCheck.arbitrary.shrinking.functor.functor
import arrow.propCheck.arbitrary.smart.arbitrary.arbitrary
import arrow.propCheck.arbitrary.smart.functor.functor
import arrow.propCheck.instances.arbitrary
import arrow.propCheck.kotlintest.UnitSpec
import arrow.test.generators.functionAToBArbitrary
import arrow.test.laws.EqLaws
import arrow.test.laws.FunctorLaws
import arrow.test.laws.ShowLaws
import arrow.typeclasses.Eq

class BlindSpec : UnitSpec() {
  init {
    val blindKindArb = Blind.arbitrary(Int.arbitrary()).let {
      Arbitrary(it.arbitrary().map { it as Kind<ForBlind, Int> }) { fail -> it.shrink(fail.fix()) }
    }
    testLaws(
      FunctorLaws.laws(Blind.functor(), blindKindArb, functionAToBArbitrary(Int.arbitrary()), Eq { a, b ->
        Blind.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
      }),
      EqLaws.laws(Blind.eq(Int.eq()), Blind.arbitrary(Int.arbitrary()))
    )

    "Blind should display as (*) with show" {
      forAll { i: Blind<Int> ->
        Blind.show<Int>().run { i.show() }.eqv("(*)")
      }
    }
  }
}

class FixedSpec : UnitSpec() {
  init {
    val fixedKindArb = Fixed.arbitrary(Int.arbitrary()).let {
      Arbitrary(it.arbitrary().map { it as Kind<ForFixed, Int> }) { fail -> it.shrink(fail.fix()) }
    }
    testLaws(
      FunctorLaws.laws(Fixed.functor(), fixedKindArb, functionAToBArbitrary(Int.arbitrary()), Eq { a, b ->
        Fixed.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
      }),
      ShowLaws.laws(Fixed.show(Int.show()), Fixed.eq(Int.eq()), Fixed.arbitrary(Int.arbitrary())),
      EqLaws.laws(Fixed.eq(Int.eq()), Fixed.arbitrary(Int.arbitrary()))
    )

    "Fixed should never be shrunk" {
      forAll { _: Fixed<Int> ->
        idempotentIOProperty(
          propCheckIO { Boolean.testable().run { false.property() } }
            .map {
              (it is Result.Failure && it.numShrinks + it.numShrinkTries == 0)
            }
        )
      }
    }
  }
}

class OrderedListSpec : UnitSpec() {
  init {
    val orderdListKindArb = OrderedList.arbitrary(Int.order(), Int.arbitrary()).let {
      Arbitrary(it.arbitrary().map { it as Kind<ForOrderedList, Int> }) { fail -> it.shrink(fail.fix()) }
    }
    testLaws(
      FunctorLaws.laws(OrderedList.functor(), orderdListKindArb, functionAToBArbitrary(Int.arbitrary()), Eq { a, b ->
        OrderedList.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
      }),
      ShowLaws.laws(OrderedList.show(Int.show()), OrderedList.eq(Int.eq()), OrderedList.arbitrary(Int.order(), Int.arbitrary())),
      EqLaws.laws(OrderedList.eq(Int.eq()), OrderedList.arbitrary(Int.order(), Int.arbitrary()))
    )
    "OrderedList arbitrary should only return ordered lists" {
      forAll(OrderedList.arbitrary(Int.order(), Int.arbitrary())) { (l) ->
        checkCoverage(
          cover(85.0, l.size > 1, "non-trivial",
            ListK.eq(Int.eq()).run {
              l.sorted().k().eqv(l.k())
            }
          )
        )
      }
    }
    "OrderedList arbitrary should only shrink to ordered lists" {
      forAll(OrderedList.arbitrary(Int.order(), Int.arbitrary())) { oL ->
        OrderedList.arbitrary(Int.order(), Int.arbitrary()).shrink(oL)
          .fold(true) { acc, (l) ->
            acc && ListK.eq(Int.eq()).run {
              l.sorted().k().eqv(l.k())
            }
          }
      }
    }
  }
}

class SmartSpec : UnitSpec() {
  init {
    val smartKindArb = Smart.arbitrary(Int.arbitrary()).let {
      Arbitrary(it.arbitrary().map { it as Kind<ForSmart, Int> }) { fail -> it.shrink(fail.fix()) }
    }
    testLaws(
      FunctorLaws.laws(Smart.functor(), smartKindArb, functionAToBArbitrary(Int.arbitrary()), Eq { a, b ->
        val aF = a.fix();
        val bF = b.fix()
        aF.a == bF.a && aF.i == bF.i
      })
    )
  }
}

class Shrink2Spec : UnitSpec() {
  init {
    val shrink2KindArb = Shrink2.arbitrary(Int.arbitrary()).let {
      Arbitrary(it.arbitrary().map { it as Kind<ForShrink2, Int> }) { fail -> it.shrink(fail.fix()) }
    }
    testLaws(
      FunctorLaws.laws(Shrink2.functor(), shrink2KindArb, functionAToBArbitrary(Int.arbitrary()), Eq { a, b ->
        Shrink2.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
      }),
      ShowLaws.laws(Shrink2.show(Int.show()), Shrink2.eq(Int.eq()), Shrink2.arbitrary(Int.arbitrary())),
      EqLaws.laws(Shrink2.eq(Int.eq()), Shrink2.arbitrary(Int.arbitrary()))
    )

    "shrink2 should perform 2 shrinking steps instead of just one" {
      val shrinkS = object : ShrinkState<Int, Int> {
        override fun shrinkInit(a: Int): Int = 0
        override fun shrinkState(a: Int, state: Int): Sequence<Tuple2<Int, Int>> =
          (state + 1).let { sequenceOf(it toT it) }
      }
      forAll(Shrink2.arbitrary(Shrinking.arbitrary(Int.arbitrary(), shrinkS))) { i ->
        Shrink2.arbitrary(Shrinking.arbitrary(Int.arbitrary(), shrinkS)).shrink(i)
          .fold(false) { acc, (v) -> acc || v.a == 2 }
      }
    }
  }
}

class ShrinkingSpec : UnitSpec() {
  init {
    val IntShrinkState = object : ShrinkState<Int, Int> {
      override fun shrinkInit(a: Int): Int = 0
      override fun shrinkState(a: Int, state: Int): Sequence<Tuple2<Int, Int>> =
        (state + 1).let { sequenceOf(it toT it) }
    }
    val shrinkingKindArb = Shrinking.arbitrary(Int.arbitrary(), IntShrinkState).let {
      Arbitrary(it.arbitrary().map { it as Kind<ShrinkingPartialOf<Int>, Int> }) { fail -> it.shrink(fail.fix()) }
    }
    testLaws(
      FunctorLaws.laws(Shrinking.functor(), shrinkingKindArb, functionAToBArbitrary(Int.arbitrary()), Eq { a, b ->
        val aF = a.fix();
        val bF = b.fix()
        aF.a == bF.a && aF.s == bF.s
      })
    )

    "shrinking should shrink with keeping state" {
      forAll(Shrinking.arbitrary(Int.arbitrary(), IntShrinkState)) { s ->
        Shrinking.arbitrary(Int.arbitrary(), IntShrinkState).shrink(s).fold(true) { acc, (s, a) ->
          acc && s == 1 && s == a
        }
      }
    }
  }
}

class PositiveSpec : UnitSpec() {
  init {
    val positiveKindArb = Positive.arbitrary(Int.arbitrary()).let {
      Arbitrary(it.arbitrary().map { it as Kind<ForPositive, Int> }) { fail -> it.shrink(fail.fix()) }
    }
    testLaws(
      FunctorLaws.laws(Positive.functor(), positiveKindArb, functionAToBArbitrary(Int.arbitrary()), Eq { a, b ->
        Positive.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
      }),
      ShowLaws.laws(Positive.show(Int.show()), Positive.eq(Int.eq()), Positive.arbitrary(Int.arbitrary())),
      EqLaws.laws(Positive.eq(Int.eq()), Positive.arbitrary(Int.arbitrary()))
    )

    "positive arbitrary should only generate positive numbers" {
      forAll { (i): Positive<Int> ->
        i > 0
      }
    }
    "positive arbitrary should only shrink to positive numbers" {
      forAll { pI: Positive<Int> ->
        Positive.arbitrary(Int.arbitrary()).shrink(pI).fold(true) { acc, (i) ->
          acc && i > 0
        }
      }
    }
  }
}

class NonNegativeSpec : UnitSpec() {
  init {
    val nonNegativeKindArb = NonNegative.arbitrary(Int.arbitrary()).let {
      Arbitrary(it.arbitrary().map { it as Kind<ForNonNegative, Int> }) { fail -> it.shrink(fail.fix()) }
    }
    testLaws(
      FunctorLaws.laws(NonNegative.functor(), nonNegativeKindArb, functionAToBArbitrary(Int.arbitrary()), Eq { a, b ->
        NonNegative.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
      }),
      ShowLaws.laws(NonNegative.show(Int.show()), NonNegative.eq(Int.eq()), NonNegative.arbitrary(Int.arbitrary())),
      EqLaws.laws(NonNegative.eq(Int.eq()), NonNegative.arbitrary(Int.arbitrary()))
    )

    "NonNegative arbitrary should only generate nonNegative numbers" {
      forAll { (i): NonNegative<Int> ->
        i >= 0
      }
    }
    "NonNegative arbitrary should only shrink to nonNegative numbers" {
      forAll { pI: NonNegative<Int> ->
        NonNegative.arbitrary(Int.arbitrary()).shrink(pI).fold(true) { acc, (i) ->
          acc && i >= 0
        }
      }
    }
  }
}

class NegativeSpec : UnitSpec() {
  init {
    val negativeKindArb = Negative.arbitrary(Int.arbitrary()).let {
      Arbitrary(it.arbitrary().map { it as Kind<ForNegative, Int> }) { fail -> it.shrink(fail.fix()) }
    }
    testLaws(
      FunctorLaws.laws(Negative.functor(), negativeKindArb, functionAToBArbitrary(Int.arbitrary()), Eq { a, b ->
        Negative.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
      }),
      ShowLaws.laws(Negative.show(Int.show()), Negative.eq(Int.eq()), Negative.arbitrary(Int.arbitrary())),
      EqLaws.laws(Negative.eq(Int.eq()), Negative.arbitrary(Int.arbitrary()))
    )

    "Negative arbitrary should only generate Negative numbers" {
      forAll { (i): Negative<Int> ->
        i < 0
      }
    }
    "Negative arbitrary should only shrink to Negative numbers" {
      forAll { pI: Negative<Int> ->
        Negative.arbitrary(Int.arbitrary()).shrink(pI).fold(true) { acc, (i) ->
          acc && i < 0
        }
      }
    }
  }
}

class NonPositiveSpec : UnitSpec() {
  init {
    val nonPositiveKindArb = NonPositive.arbitrary(Int.arbitrary()).let {
      Arbitrary(it.arbitrary().map { it as Kind<ForNonPositive, Int> }) { fail -> it.shrink(fail.fix()) }
    }
    testLaws(
      FunctorLaws.laws(NonPositive.functor(), nonPositiveKindArb, functionAToBArbitrary(Int.arbitrary()), Eq { a, b ->
        NonPositive.eq(Int.eq()).run { a.fix().eqv(b.fix()) }
      }),
      ShowLaws.laws(NonPositive.show(Int.show()), NonPositive.eq(Int.eq()), NonPositive.arbitrary(Int.arbitrary())),
      EqLaws.laws(NonPositive.eq(Int.eq()), NonPositive.arbitrary(Int.arbitrary()))
    )

    "NonPositive arbitrary should only generate NonPositive numbers" {
      forAll { (i): NonPositive<Int> ->
        i <= 0
      }
    }

    "NonPositive arbitrary should only shrink to NonPositive numbers" {
      forAll { pI: NonPositive<Int> ->
        NonPositive.arbitrary(Int.arbitrary()).shrink(pI).fold(true) { acc, (i) ->
          acc && i <= 0
        }
      }
    }
  }
}