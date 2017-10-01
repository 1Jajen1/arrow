package kategory.laws

import io.kotlintest.properties.Gen
import io.kotlintest.properties.forAll
import kategory.*

object EqLaws {

    inline fun <reified F> laws(EQ: Eq<F> = eq<F>(), noinline cf: (Int) -> F): List<Law> =
            listOf(
                    Law("Eq Laws: commutativity", { commutativeEquality(EQ, cf) })
            )

    inline fun <reified F> commutativeEquality(EQ: Eq<F> = eq(), crossinline cf: (Int) -> F): Unit =
            forAll(Gen.int(), { int: Int ->
                val a = cf(int)
                val b = cf(int)
                EQ.eqv(a, b) == EQ.eqv(b, a)
            })
}