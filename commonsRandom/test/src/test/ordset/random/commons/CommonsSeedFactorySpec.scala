package test.ordset.random.commons

import ordset.random.commons.CommonsSeedFactory
import org.scalatest.funspec.AnyFunSpec
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

/**
 * Current library is a thin wrapper for [[org.apache.commons.rng]].
 * We don't need here to check quality of random generators as it's already provided by underling implementation.
 */
@RunWith(classOf[JUnitRunner])
class CommonsSeedFactorySpec extends AnyFunSpec {

  it("should generate random seeds") {
    val size = 10
    assert(Range(0, size).map(_ => CommonsSeedFactory.createInt()).toSet.size > 1)
    assert(Range(0, size).map(_ => CommonsSeedFactory.createLong()).toSet.size > 1)
    assert(Range(0, size).map(_ => CommonsSeedFactory.createIntArray(5)).toSet.size > 1)
    assert(Range(0, size).map(_ => CommonsSeedFactory.createLongArray(5)).toSet.size > 1)
  }
}
