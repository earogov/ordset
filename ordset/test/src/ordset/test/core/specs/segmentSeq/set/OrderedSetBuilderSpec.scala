package ordset.test.core.specs.segmentSeq.set

import org.junit.runner.RunWith
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.junit.JUnitRunner
import ordset.test.core.behaviors.segmentSeq.set.SetBuilderBehaviors

@RunWith(classOf[JUnitRunner])
class OrderedSetBuilderSpec extends AnyFunSpec
  with SetBuilderBehaviors {
  
  it should behave like orderedSetIsCreatedByBuilder
}
