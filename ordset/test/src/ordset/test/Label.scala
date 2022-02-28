package ordset.test

trait Label

object Label {

  def label(name: String): SimpleLabel = SimpleLabel(name)

  def simpleLabel(name: String): SimpleLabel = SimpleLabel(name)

  def sampleLabel(name: String): SampleLabel = SampleLabel(name)

  def caseLabel(name: String): CaseLabel = CaseLabel(name)

  def intSeedLabel(seed: Int): IntSeedLabel = IntSeedLabel(seed)

  def longSeedLabel(seed: Long): LongSeedLabel = LongSeedLabel(seed)

  def stringSeedLabel(seed: String): StringSeedLabel = StringSeedLabel(seed)

  def showCaseSet(labels: Set[Label]): String = labels.mkString("case(", ", ", ")")

  def showPackageSet(labels: Set[Label]): String = labels.mkString("package(", ", ", ")")

  case class SimpleLabel(name: String) extends Label {

    override def toString(): String = name
  }

  case class SampleLabel(name: String) extends Label {

    override def toString(): String = s"sample $name"
  }

  case class CaseLabel(name: String) extends Label {

    override def toString(): String = s"case $name"
  }

  trait SeedLabel extends Label {

    override def toString(): String = s"seed $seedString"

    protected def seedString: String
  }

  case class IntSeedLabel(seed: Int) extends SeedLabel {

    protected override def seedString: String = seed.toString
  }

  case class LongSeedLabel(seed: Long) extends SeedLabel {

    protected override def seedString: String = seed.toString
  }

  case class StringSeedLabel(seed: String) extends SeedLabel {

    protected override def seedString: String = seed
  }
}
