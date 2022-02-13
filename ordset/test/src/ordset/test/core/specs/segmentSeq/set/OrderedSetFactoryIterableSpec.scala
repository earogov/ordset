package ordset.test.core.specs.segmentSeq.set

import org.junit.runner.RunWith
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.junit.JUnitRunner
import ordset.test.core.behaviors.segmentSeq.FactoryValidationBehaviors

@RunWith(classOf[JUnitRunner])
class OrderedSetFactoryIterableSpec extends AnyFunSpec
  with FactoryValidationBehaviors {
  
  it should behave like sequenceOfBoundsForOrderedSetIsValidated
}
