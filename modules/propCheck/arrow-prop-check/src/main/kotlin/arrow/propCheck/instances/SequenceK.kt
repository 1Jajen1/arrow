package arrow.propCheck.instances

import arrow.data.SequenceK
import arrow.data.k
import arrow.extension
import arrow.propCheck.arbitrary.Arbitrary
import arrow.propCheck.arbitrary.Gen
import arrow.propCheck.arbitrary.shrinkList

@extension
interface SequenceKArbitrary<A> : Arbitrary<SequenceK<A>> {
  fun AA(): Arbitrary<A>
  override fun arbitrary(): Gen<SequenceK<A>> = AA().arbitrary().listOf().map { it.asSequence().k() }
  override fun shrink(fail: SequenceK<A>): Sequence<SequenceK<A>> = shrinkList(fail.toList()) { AA().shrink(it) }.map {
    it.asSequence().k()
  }
}