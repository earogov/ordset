package ordset.test.core.specs.segmentSeq.map

import org.junit.runner.RunWith
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.junit.JUnitRunner
import ordset.test.core.behaviors.segmentSeq.FactoryValidationBehaviors

@RunWith(classOf[JUnitRunner])
class OrderedMapFactoryIterableSpec extends AnyFunSpec
  with FactoryValidationBehaviors {
  
  it should behave like sequenceOfBoundsForOrderedMapIsValidated
}
