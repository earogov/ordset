package ordset.test.random.commons.specs

import ordset.random.commons.{CommonsRngFactory, KISSRngFactory}
import org.apache.commons.rng.simple.RandomSource
import org.scalatest.funspec.AnyFunSpec
import ordset.test.random.commons.TestRngUtil.{assertDifferentRng, assertSameRng, assertSaneRng}
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

/**
 * Current library is a thin wrapper for [[org.apache.commons.rng]].
 * We don't need here to check quality of random generators as it's already provided by underling implementation.
 */
@RunWith(classOf[JUnitRunner])
class CommonsRngFactorySpec extends AnyFunSpec {

  it("should create KISS random generator") {
    assertSaneRng(KISSRngFactory.create())
    assertSaneRng(KISSRngFactory.create(0xa19f0b2d0c48ea51L, 0x0e87fa311d2d0fL))

    // Make sure that seeds are passed.
    assertSameRng(
      KISSRngFactory.create(6060L, 7070L),
      KISSRngFactory.create(6060L, 7070L)
    )

    assertDifferentRng(
      KISSRngFactory.create(6060L, 1111L),
      KISSRngFactory.create(6060L, 2222L)
    )

    assertDifferentRng(
      KISSRngFactory.create(1111L, 6060L),
      KISSRngFactory.create(2222L, 6060L)
    )
  }

  it("should create specified random generator") {
    assertSaneRng(CommonsRngFactory.create(RandomSource.XO_SHI_RO_128_SS))
    assertSaneRng(CommonsRngFactory.create(RandomSource.XO_SHI_RO_128_SS, 4321))
    assertSaneRng(CommonsRngFactory.create(RandomSource.TWO_CMRES_SELECT, 2222, 1, 3))

    // Make sure that seed and additional args are passed.
    assertSameRng(
      CommonsRngFactory.create(RandomSource.TWO_CMRES, 4321),
      CommonsRngFactory.create(RandomSource.TWO_CMRES, 4321)
    )

    assertDifferentRng(
      CommonsRngFactory.create(RandomSource.TWO_CMRES_SELECT, 2222, 1, 3),
      CommonsRngFactory.create(RandomSource.TWO_CMRES_SELECT, 3333, 1, 3)
    )

    assertSameRng(
      CommonsRngFactory.create(RandomSource.TWO_CMRES_SELECT, 3333, 3, 5),
      CommonsRngFactory.create(RandomSource.TWO_CMRES_SELECT, 3333, 3, 5)
    )

    assertDifferentRng(
      CommonsRngFactory.create(RandomSource.TWO_CMRES_SELECT, 3333, 1, 3),
      CommonsRngFactory.create(RandomSource.TWO_CMRES_SELECT, 3333, 3, 5)
    )
  }
}
