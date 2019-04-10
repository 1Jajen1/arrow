package arrow.test.laws

import arrow.propCheck.Property
import arrow.typeclasses.Eq

fun throwableEq() = Eq { a: Throwable, b ->
  a::class == b::class && a.message == b.message
}

data class Law (val name: String, val test: Property)
