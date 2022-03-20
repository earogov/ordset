package ordset.test.core.specs.segmentSeq.map

import org.junit.runner.RunWith
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.junit.JUnitRunner
import ordset.test.core.behaviors.segmentSeq.map.MapBuilderBehaviors

@RunWith(classOf[JUnitRunner])
class OrderedMapBuilderSpec extends AnyFunSpec
  with MapBuilderBehaviors {
  
  it should behave like orderedMapIsCreatedByBuilder
}
