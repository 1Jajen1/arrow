package arrow.propCheck.arbitrary

import arrow.data.extensions.sequence.foldable.isEmpty
import arrow.propCheck.*
import arrow.propCheck.kotlintest.PropertySpec

class IntegralShrinkSpec : PropertySpec({
    "shrinking 0 should return an empty list (Byte)" {
        once(
          shrinkByte(0).isEmpty()
        )
    }
    "shrinking 0 should return an empty list (Int)" {
        once(
          shrinkInt(0).isEmpty()
        )
    }
    "shrinking 0 should return an empty list (Long)" {
        once(
          shrinkInt(0).isEmpty()
        )
    }
    "shrinking 0 should return an empty list (Float)" {
        once(
          shrinkFloat(0F).isEmpty()
        )
    }
    "shrinking 0 should return an empty list (Double)" {
        once(
          shrinkDouble(0.0).isEmpty()
        )
    }

    "shrinking ints should yield a sorted list of integers"(Args(maxSuccess = 10000)) {
        forAll { i: Int ->
            shrinkInt(i).take(100).toList().let {
                counterexample(
                    { "$it" },
                    it.sorted() == it || it.sorted().reversed() == it
                )
            }
        }
    }
    "shrinking longs should yield a sorted list of longs"(Args(maxSuccess = 10000)) {
        forAll { i: Long ->
            shrinkLong(i).take(100).toList().let {
                counterexample(
                    { "$it" },
                    it.sorted() == it || it.sorted().reversed() == it
                )
            }
        }
    }
    "shrinking bytes should yield a sorted list of byte"(Args(maxSuccess = 10000)) {
        forAll { i: Byte ->
            shrinkByte(i).take(100).toList().let {
                counterexample(
                    { "$it" },
                    it.sorted() == it || it.sorted().reversed() == it
                )
            }
        }
    }
})

class ShrinkListSpec : PropertySpec({
    "shrinkList should generate an ordered sequence of smaller lists"(Args(maxSuccess = 1000)) {
        forAll { l: List<Int> ->
            shrinkList(l, ::shrinkInt).take(100).toList().let {
                counterexample(
                    { "$it" },
                    it.zipWithNext().fold(true) { acc, (l, r) ->
                        acc && (l.size <= r.size)
                    }
                )
            }
        }
    }
})