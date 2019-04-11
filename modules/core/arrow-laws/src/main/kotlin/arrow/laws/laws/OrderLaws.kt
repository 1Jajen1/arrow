package arrow.laws.laws

import arrow.core.Tuple2
import arrow.core.Tuple3
import arrow.core.toT
import arrow.laws.generators.smallRangeArb
import arrow.propCheck.*
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.instances.tuple2.arbitrary.arbitrary
import arrow.propCheck.instances.tuple3.arbitrary.arbitrary
import arrow.typeclasses.Order

object OrderLaws {

  fun <F> laws(O: Order<F>, arbF: Arbitrary<F>): List<Law> {
    return EqLaws.laws(O, arbF) + listOf(
      Law("Order law: reflexivity partial order", O.reflexivityPartialOrder(arbF)),
      Law("Order law: antisymmetry partial order", O.antisymmetryPartialOrder(arbF)),
      Law("Order law: transitivity partial order", O.transitivityPartialOrder(arbF)),
      Law("Order law: greater than or equal partial order", O.greaterThanOrEqualPartialOrder(arbF)),
      Law("Order law: lesser than partial order", O.lesserThanPartialOrder(arbF)),
      Law("Order law: greater than partial order", O.greaterThanPartialOrder(arbF)),
      Law("Order law: totality order", O.totalityOrder(arbF)),
      Law("Order law: compare order", O.compareOrder(arbF)),
      Law("Order law: min order", O.minOrder(arbF)),
      Law("Order law: max order", O.maxOrder(arbF)),
      Law("Order law: operator compareTo delegates to compare order", O.operatorCompareToOrder(arbF))
    )
  }

  fun <F> Order<F>.reflexivityPartialOrder(arbF: Arbitrary<F>) =
    forAll(arbF.arbitrary(), Boolean.testable()) { x ->
      x.lte(x)
    }

  /**
   * From here on out coverage checks are used to ensure that the tested cases
   * are actually hit somewhat frequently
   * TODO review precentages
   */
  fun <F> Order<F>.antisymmetryPartialOrder(arbF: Arbitrary<F>) =
    forAll(Tuple2.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF))) { (x, y) ->
      checkCoverage(
        cover(10.0, x.eqv(y), "x == y",
          !(x.lte(y) && y.lte(x)) || x.eqv(y)
        )
      )
    }

  fun <F> Order<F>.transitivityPartialOrder(arbF: Arbitrary<F>) =
    forAll(Tuple3.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF), smallRangeArb(arbF))) { (x, y, z) ->
      checkCoverage(
        cover(10.0, x.lte(y) && y.lte(z), "x < y && y < z",
          !(x.lte(y) && y.lte(z)) || x.lte(z)
        )
      )
    }

  fun <F> Order<F>.greaterThanOrEqualPartialOrder(arbF: Arbitrary<F>) =
    forAll(Tuple2.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF))) { (x, y) ->
      checkCoverage(
        // we'd probably want the majority of cases to not be x == y
        cover(70.0, x.neqv(y), "x != y",
          x.lte(y) == y.gte(x)
        )
      )
    }

  fun <F> Order<F>.lesserThanPartialOrder(arbF: Arbitrary<F>) =
    forAll(Tuple2.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF))) { (x, y) ->
      checkCoverage(
        cover(10.0, x.lt(y), "x < y",
          x.lt(y) == (x.lte(y) && x.neqv(y))
        )
      )
    }

  fun <F> Order<F>.greaterThanPartialOrder(arbF: Arbitrary<F>) =
    forAll(Tuple2.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF))) { (x, y) ->
      checkCoverage(
        cover(10.0, x.neqv(y), "x < y",
          x.lt(y) == y.gt(x)
        )
      )
    }

  fun <F> Order<F>.totalityOrder(arbF: Arbitrary<F>) =
    forAll(Tuple2.arbitrary(arbF, arbF), Boolean.testable()) { (x, y) ->
      x.lte(y) || y.lte(x)
    }

  fun <F> Order<F>.compareOrder(arbF: Arbitrary<F>) =
    forAll(Tuple2.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF))) { (x, y) ->
      checkCoverage(
        coverTable(
          "Results",
          listOf("==" toT 10.0, ">" toT 30.0, "<" toT 30.0),
          x.compare(y).let { c ->
            tabulate(
              "Results",
              listOf(when {
                c < 0 -> "<"
                c > 0 -> ">"
                else -> "=="
              }),
              ((c < 0) == x.lt(y)) && ((c == 0) == x.eqv(y)) && ((c > 0) == x.gt(y))
            )
          }
        )
      )
    }

  fun <F> Order<F>.minOrder(arbF: Arbitrary<F>) =
    forAll(Tuple2.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF))) { (x, y) ->
      val c = x.compare(y)
      val m = x.min(y)
      checkCoverage(
        coverTable(
          "Results",
          listOf("min = x = y" toT 10.0, "min = x" toT 30.0, "min = y" toT 30.0),
          tabulate(
            "Results",
            listOf(when {
              c < 0 -> "min = x"
              c > 0 -> "min = y"
              else -> "min = x = y"
            }),
            when {
              c < 0 -> m == x
              c > 0 -> m == y
              else -> m == x && m == y
            }
          )
        )
      )
    }

  fun <F> Order<F>.maxOrder(arbF: Arbitrary<F>) =
    forAll(Tuple2.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF))) { (x, y) ->
      val c = x.compare(y)
      val m = x.max(y)
      checkCoverage(
        coverTable(
          "Results",
          listOf("min = x = y" toT 10.0, "min = x" toT 30.0, "min = y" toT 30.0),
          tabulate(
            "Results",
            listOf(when {
              c < 0 -> "min = y"
              c > 0 -> "min = x"
              else -> "min = x = y"
            }),
            when {
              c < 0 -> m == y
              c > 0 -> m == x
              else -> m == x && m == y
            }
          )
        )
      )
    }

  fun <F> Order<F>.operatorCompareToOrder(arbF: Arbitrary<F>): Property =
    forAll(Tuple2.arbitrary(smallRangeArb(arbF), smallRangeArb(arbF))) { (x, y) ->
      val c = x.compare(y)
      checkCoverage(
        coverTable(
          "Results",
          listOf("==" toT 10.0, "<" toT 30.0, ">" toT 30.0),
          tabulate(
            "Results",
            listOf(when {
              c < 0 -> "<"
              c > 0 -> ">"
              else -> "=="
            }),
            x.compare(y) == x.compareTo(y)
          )
        )
      )
    }
}
