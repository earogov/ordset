import ordset.random.UnsafeUniformRandom
import ordset.random.commons.ApacheCommonsRng
import org.apache.commons.rng.simple.RandomSource
import org.scalatest.funspec.AnyFunSpec

/**
 * Current library is a thin wrapper for [[org.apache.commons.rng]].
 * We don't need here to check quality of random generators as it's already provided by underling implementation.
 */
class ApacheCommonsRngSpec extends AnyFunSpec {

  it("should create default random generator") {
    testRng(ApacheCommonsRng.default())
    testRng(ApacheCommonsRng.default(1234L))

    // Make sure that seed is passed.
    assertSame(
      ApacheCommonsRng.default(6060L),
      ApacheCommonsRng.default(6060L)
    )

    assertDifferent(
      ApacheCommonsRng.default(6060L),
      ApacheCommonsRng.default(5577L)
    )
  }

  it("should create specified random generator") {
    testRng(ApacheCommonsRng.create(RandomSource.XO_SHI_RO_128_SS))
    testRng(ApacheCommonsRng.create(RandomSource.XO_SHI_RO_128_SS, 4321))
    testRng(ApacheCommonsRng.create(RandomSource.TWO_CMRES_SELECT, 2222, 1, 3))

    // Make sure that seed and additional args are passed.
    assertSame(
      ApacheCommonsRng.create(RandomSource.TWO_CMRES, 4321),
      ApacheCommonsRng.create(RandomSource.TWO_CMRES, 4321)
    )

    assertDifferent(
      ApacheCommonsRng.create(RandomSource.TWO_CMRES_SELECT, 2222, 1, 3),
      ApacheCommonsRng.create(RandomSource.TWO_CMRES_SELECT, 3333, 1, 3)
    )

    assertSame(
      ApacheCommonsRng.create(RandomSource.TWO_CMRES_SELECT, 3333, 3, 5),
      ApacheCommonsRng.create(RandomSource.TWO_CMRES_SELECT, 3333, 3, 5)
    )

    assertDifferent(
      ApacheCommonsRng.create(RandomSource.TWO_CMRES_SELECT, 3333, 1, 3),
      ApacheCommonsRng.create(RandomSource.TWO_CMRES_SELECT, 3333, 3, 5)
    )
  }

  private def assertSame(rng1: UnsafeUniformRandom, rng2: UnsafeUniformRandom): Unit =
    Range(1, 100).foreach(i => assert(rng1.nextLong() == rng2.nextLong()))

  private def assertDifferent(rng1: UnsafeUniformRandom, rng2: UnsafeUniformRandom): Unit =
    assert(rng1.nextLong() != rng2.nextLong())

  /**
   * Calls all methods of the given generator to make sure there is no exceptions.
   */
  private def testRng (rng: UnsafeUniformRandom) {
    val bound = 100

    val int = rng.nextInt()

    val intBounded = rng.nextInt(bound)
    assert(intBounded >= 0 && intBounded < bound)

    val long = rng.nextLong()

    val longBounded = rng.nextLong(100)
    assert(longBounded >= 0 && longBounded < bound)

    val boolean = rng.nextBoolean()

    val double = rng.nextDouble()

    val float = rng.nextFloat()

    val bytes = new Array[Byte](16)
    rng.nextBytes(bytes)

    val moreBytes = new Array[Byte](32)
    rng.nextBytes(moreBytes, 15, 16)
    // Check that first half of array is empty.
    assert(Range(0, 15).map(moreBytes(_)).forall(_ == 0.toByte))
  }
}
