package arrow.propCheck.arbitrary

import arrow.core.Tuple2
import arrow.core.toT
import arrow.data.Nel
import arrow.propCheck.*
import arrow.propCheck.arbitrary.gen.monad.monad
import arrow.propCheck.kotlintest.UnitSpec
import arrow.propCheck.property.testable.testable
import arrow.laws.laws.MonadLaws
import arrow.typeclasses.Eq

class GenSpec : UnitSpec() {
  init {
    testLaws(
      MonadLaws.laws(
        Gen.monad(),
        Eq { a, b ->
          a.fix().unGen(RandSeed(0L) toT 10) == b.fix().unGen(RandSeed(0L) toT 10)
        }
      ) { this.fix().unGen(RandSeed(0) toT 0) }
    )

    "Gens with same seed and size should return the same values" {

      forAll { (l, s): Pair<Long, NonNegative<Int>> ->
        (arbitrarySizedInt().unGen(RandSeed(l) toT s.a) == arbitrarySizedInt().unGen(RandSeed(l) toT s.a))
          .property()
      }

    }

    "Gen.resize should work" {
      forAll { (i): NonNegative<Int> ->
        idempotentIOProperty(
          Gen.getSize().resize(i).generate().map { it == i }
        )
      }

    }

    "Gen.scale should work" {
      // TODO redo when generating functions is done
      forAll { (iP, jP): Tuple2<NonNegative<Int>, NonNegative<Int>> ->
        val (i) = iP; val (j) = jP
        idempotentIOProperty(
          Gen.getSize().scale { it + j }.resize(i).generate().map { it == i + j }
        )
      }

    }

    // TODO Fix stack safety of Gen so that these functions work under any circumstance
    "Gen.suchThat" {
      // TODO redo when generating functions is done
      forAll { i: Int ->
        idempotentIOProperty(
          Gen.getSize().suchThat { it > i }.generate().map { it > i }
        )
      }

    }

    "Gen.suchThatOption" {
      // TODO redo when generating functions is done

      forAll { i: Int ->
        idempotentIOProperty(
          Gen.getSize().suchThatOption { it > i }.generate().map {
            it.fold({ true }, { it > i })
          }
        )
      }

    }

    "Gen.listOf should generate lists of size 0 to i" {

      forAll { (i): NonNegative<Int> ->
        idempotentIOProperty(
          arbitrarySizedByte().listOf().resize(i).generate().map { it.size <= i }
        )
      }

    }

    "Gen.nelOf should generate Nonemptylists of size 1 to i" {

      forAll { (i): Positive<Int> ->
        idempotentIOProperty(
          arbitrarySizedByte().nelOf().resize(i).generate().map { it.size <= i }
        )
      }

    }

    "Gen.vectorOf should generate lists of size n" {

      forAll { (i): NonNegative<Int> ->
        idempotentIOProperty(
          arbitrarySizedByte().vectorOf(i).generate().map { it.size == i }
        )
      }

    }

    "Gen.sized should work" {

      forAll { (i): NonNegative<Int> ->
        idempotentIOProperty(
          Gen.sized { Gen.getSize().resize(it) }.resize(i).generate().map { it == i }
        )
      }

    }

    "Gen.getSize should work" {
      forAll { (i): NonNegative<Int> ->
        idempotentIOProperty(
          Gen.getSize().resize(i).generate().map { it == i }
        )
      }

    }

    "Gen.choose should work" {
      forAll { (a, b): Tuple2<Int, Int> ->
        val l = Math.min(a, b);
        val u = Math.max(a, b)
        idempotentIOProperty(
          Gen.choose(l toT u, Int.random()).generate().map { it in l..u }
        )
      }

    }

    "Gen.elements should return elemets of a given list" {
      forAll { l: Nel<Int> ->
        idempotentIOProperty(
          Gen.elements(*l.all.toTypedArray()).generate().map { l.contains(it) }
        )
      }

    }

    "Gen.sublistOf should return only sublists" {
      forAll { l: List<Int> ->
        idempotentIOProperty(
          Gen.sublistOf(l).generate().map { it.map { l.contains(it) }.fold(true) { acc, v -> acc && v } }
        )
      }

    }

    "Gen.shuffle should create permutations of a list" {
      forAll { l: List<Int> ->
        idempotentIOProperty(
          Gen.shuffle(l).generate().map { it.containsAll(l) }
        )
      }

    }

    // TODO I don't like the following 3 tests
    "Gen.choose should equally choose in the range" {
      forAll(
        Gen.choose(1 toT 3 + 1, Int.random()),
        Property.testable()
      ) {
        checkCoverage(
          coverTable(
            "Data",
            listOf(
              "1" toT 33.3,
              "2" toT 33.3,
              "3" toT 33.3
            ),
            tabulate(
              "Data",
              listOf("$it"),
              true
            )
          )
        )
      }

    }

    "Gen.elements should equally choose between the elements" {

      forAll(
        Gen.elements(1, 2, 3),
        Property.testable()
      ) {
        checkCoverage(
          coverTable(
            "Data",
            listOf(
              "1" toT 33.3,
              "2" toT 33.3,
              "3" toT 33.3
            ),
            tabulate(
              "Data",
              listOf("$it"),
              true
            )
          )
        )
      }

    }
    "Gen.oneOf should equally choose between the generators" {
      forAll(
        Gen.oneOf(
          Gen.elements(1),
          Gen.elements(2),
          Gen.elements(3)
        ),
        Property.testable()
      ) {
        checkCoverage(
          coverTable(
            "Data",
            listOf(
              "1" toT 33.3,
              "2" toT 33.3,
              "3" toT 33.3
            ),
            tabulate(
              "Data",
              listOf("$it"),
              true
            )
          )
        )
      }
    }
  }
}