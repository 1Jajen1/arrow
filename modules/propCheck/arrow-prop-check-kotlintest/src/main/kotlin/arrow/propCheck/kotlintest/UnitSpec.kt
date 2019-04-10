package arrow.propCheck.kotlintest

import arrow.propCheck.propCheck
import arrow.test.laws.Law
import io.kotlintest.TestCase
import io.kotlintest.TestType

/**
 * Base class for unit tests
 */
abstract class UnitSpec : AbstractPropertySpec() {

  private val lawTestCases = mutableListOf<TestCase>()

  fun testLaws(vararg laws: List<Law>): List<TestCase> = laws
    .flatMap { list: List<Law> -> list.asIterable() }
    // TODO replace ones all occurances of duplicate tests are found (in the old version some tests got skipped because
    //    of this, including distinct tests!)
    .groupBy { it.name }
    .entries.map { (_, v) -> if (v.size > 1) v.mapIndexed { i, law -> law.copy(name = "${law.name}_$i") } else v  }.flatMap { it }
    .map { law: Law ->
      val lawTestCase = createTestCase(
        law.name,
        { propCheck { law.test } },
        defaultTestCaseConfig,
        TestType.Test
      )
      lawTestCases.add(lawTestCase)
      lawTestCase
    }

  override fun testCases(): List<TestCase> = super.testCases() + lawTestCases

}
