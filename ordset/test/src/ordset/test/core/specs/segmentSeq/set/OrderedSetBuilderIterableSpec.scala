package ordset.test.core.specs.segmentSeq.set

import org.junit.runner.RunWith
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.junit.JUnitRunner
import ordset.test.core.behaviors.segmentSeq.BuilderValidationBehaviors

@RunWith(classOf[JUnitRunner])
class OrderedSetBuilderIterableSpec extends AnyFunSpec
  with BuilderValidationBehaviors {
  
  it should behave like sequenceOfIntervalsForOrderedSetIsValidated
}
