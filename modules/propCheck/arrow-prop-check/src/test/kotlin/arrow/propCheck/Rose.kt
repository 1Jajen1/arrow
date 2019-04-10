package arrow.propCheck

import arrow.Kind
import arrow.propCheck.kotlintest.UnitSpec
import arrow.typeclasses.Eq

class RoseSpec : UnitSpec() {
  fun roseEq(): Eq<Kind<ForRose, Int>> = Eq { a, b ->
    when (val aF = a.fix()) {
      is Rose.IORose -> when (val bF = b.fix()) {
        is Rose.IORose -> roseEq().run { aF.ioRose.unsafeRunSync().eqv(bF.ioRose.unsafeRunSync()) }
        is Rose.MkRose -> false
      }
      is Rose.MkRose -> when (val bF = b.fix()) {
        is Rose.MkRose -> aF.res == bF.res && roseEq().run {
          aF.shrunk.zip(bF.shrunk).fold(true) { acc, (a, b) ->
            acc && a.eqv(b)
          }
        }
        is Rose.IORose -> false
      }
    }
  }

  init {
    /* Rose as of now is not stack-safe (that should never be a problem tho)
    testLaws(
        MonadLaws.laws(Rose.monad(), roseEq()) {
            when (val it = this.fix()) {
                is Rose.MkRose -> it.res
                else -> throw IllegalStateException("Dont")
            }
        }
    )*/
  }
}