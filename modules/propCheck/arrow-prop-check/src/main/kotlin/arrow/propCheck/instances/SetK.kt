package arrow.propCheck.instances

import arrow.data.SetK
import arrow.data.k
import arrow.extension
import arrow.typeclasses.Show
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.arbitrary.Gen
import arrow.propCheck.arbitrary.shrinkList

@extension
interface SetKArbitrary<A> : Arbitrary<SetK<A>> {
    fun AA(): Arbitrary<A>
    override fun arbitrary(): Gen<SetK<A>> = AA().arbitrary().listOf().map { it.toSet().k() }
    override fun shrink(fail: SetK<A>): Sequence<SetK<A>> = shrinkList(fail.toList()) { AA().shrink(it) }.map { it.toSet().k() }
}

interface SetKShow<A> : Show<SetK<A>> {
    fun SA(): Show<A>
    override fun SetK<A>.show(): String =
            "Set(" + joinToString { SA().run { it.show() } } + ")"
}