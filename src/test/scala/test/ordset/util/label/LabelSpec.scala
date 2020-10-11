package test.ordset.util.label

import ordset.util.label.Label
import org.scalatest.funspec.AnyFunSpec

class LabelSpec extends AnyFunSpec {

  it("should support unordered composition") {
    assert(Label("A") <+> Label("B") <+> Label("C") == Label("A") <+> Label("B") <+> Label("C"))
    assert(Label("A") <+> Label("B") <+> Label("C") == Label("B") <+> Label("A") <+> Label("C"))
    assert(Label("A") <+> Label("B") <+> Label("C") == Label("B") <+> Label("C") <+> Label("A"))
    assert(Label("A") <+> Label("B") <+> Label("C") == Label("A") <+> Label("C") <+> Label("B"))
    assert(Label("A") <+> Label("B") <+> Label("C") == Label("C") <+> Label("A") <+> Label("B"))
    assert(Label("A") <+> Label("B") <+> Label("C") == Label("C") <+> Label("B") <+> Label("A"))

    assert(Label("A") <+> Label("A") == Label("A"))
    assert((Label("A") <+> Label("A")) <+> Label("A") == Label("A"))
  }

  it("should support ordered composition") {
    assert(Label("A") +> Label("B") +> Label("C") == Label("A") +> Label("B") +> Label("C"))
    assert(Label("A") +> Label("B") +> Label("C") != Label("B") +> Label("A") +> Label("C"))
    assert(Label("A") +> Label("B") +> Label("C") != Label("B") +> Label("C") +> Label("A"))
    assert(Label("A") +> Label("B") +> Label("C") != Label("A") +> Label("C") +> Label("B"))
    assert(Label("A") +> Label("B") +> Label("C") != Label("C") +> Label("A") +> Label("B"))
    assert(Label("A") +> Label("B") +> Label("C") != Label("C") +> Label("B") +> Label("A"))

    assert(Label("A") +> Label("A") != Label("A"))
    assert((Label("A") +> Label("A")) +> Label("A") != Label("A"))
  }

  it("should support mixed composition") {
    assert((Label("A") +> Label("B")) <+> Label("C") == (Label("A") +> Label("B")) <+> Label("C"))
    assert((Label("A") +> Label("B")) <+> Label("C") == Label("C") <+> (Label("A") +> Label("B")))
    assert((Label("A") +> Label("B")) <+> Label("C") != (Label("B") +> Label("A")) <+> Label("C"))

    assert((Label("A") <+> Label("B")) +> Label("C") == (Label("A") <+> Label("B")) +> Label("C"))
    assert((Label("A") <+> Label("B")) +> Label("C") == (Label("B") <+> Label("A")) +> Label("C"))
    assert((Label("A") <+> Label("B")) +> Label("C") != Label("C") +> (Label("A") <+> Label("B")))
  }

  it("label set should not be equal to seq") {
    assert(Label("A") <+> Label("B") != Label("A") +> Label("B"))
    assert(Label("A") <+> Label("B") <+> Label("C") != Label("A") +> Label("B") +> Label("C"))
  }

  it("empty label should be a zero element for all composition operations") {
    val unitLabel = Label("A")
    val labelSet = Label("A") <+> Label("B")
    val labelSeq = Label("A") +> Label("B")

    assert(Label.empty <+> Label.empty == Label.empty)

    assert(Label.empty <+> unitLabel == unitLabel)
    assert(unitLabel <+> Label.empty == unitLabel)

    assert(Label.empty <+> labelSet == labelSet)
    assert(labelSet <+> Label.empty == labelSet)

    assert(Label.empty <+> labelSeq == labelSeq)
    assert(labelSeq <+> Label.empty == labelSeq)

    assert(Label.empty +> Label.empty == Label.empty)

    assert(Label.empty +> unitLabel == unitLabel)
    assert(unitLabel +> Label.empty == unitLabel)

    assert(Label.empty +> labelSet == labelSet)
    assert(labelSet +> Label.empty == labelSet)

    assert(Label.empty +> labelSeq == labelSeq)
    assert(labelSeq +> Label.empty == labelSeq)
  }

  it("should return tokens list") {
    import ordset.util.label.{Token => T}

    // empty
    assert(Label.empty.tokens == LazyList())

    // unit
    assert(Label("A").tokens == LazyList(T("A")))

    // empty composition
    assert((Label.empty <+> Label("A") <+> Label.empty).tokens == LazyList(T("A")))
    assert((Label.empty +> Label("A") +> Label.empty).tokens == LazyList(T("A")))

    // self composition
    assert((Label("A") <+> Label("A")).tokens == LazyList(T("A")))
    assert((Label("A") +> Label("A")).tokens == LazyList(T.SeqOpen, T("A"), T.SeqSeparator, T("A"), T.SeqClose))

    // unordered composition
    assert((Label("A") <+> Label("B") <+> Label("C")).tokens ==
      LazyList(T.SetOpen, T("A"), T.SetSeparator, T("B"), T.SetSeparator, T("C"), T.SetClose)
    )

    // ordered composition
    assert((Label("A") +> Label("B") +> Label("C")).tokens ==
      LazyList(T.SeqOpen, T("A"), T.SeqSeparator, T("B"), T.SeqSeparator, T("C"), T.SeqClose)
    )

    // mixed composition
    assert(((Label("A") <+> Label("B")) +> Label("C")).tokens ==
      LazyList(T.SeqOpen, T.SetOpen, T("A"), T.SetSeparator, T("B"), T.SetClose, T.SeqSeparator, T("C"), T.SeqClose)
    )
    assert(((Label("A") +> Label("B")) <+> Label("C")).tokens ==
      LazyList(T.SetOpen, T.SeqOpen, T("A"), T.SeqSeparator, T("B"), T.SeqClose, T.SetSeparator, T("C"), T.SetClose)
    )
  }

  it("should be converted to string in label builder style") {
    val show = Label.labelBuilderShow
    assert(show.show(Label.empty) == "")
    assert(show.show(Label("A")) == "A")
    assert(show.show(Label("A") <+> Label("B")) == "(A <+> B)")
    assert(show.show(Label("A") +> Label("B")) == "(A +> B)")
    assert(show.show((Label("C") <+> Label("B")) <+> Label("A")) == "(A <+> B <+> C)")
    assert(show.show((Label("C") +> Label("B")) +> Label("A")) == "(C +> B +> A)")
    assert(show.show((Label("C") <+> Label("B")) +> Label("A")) == "((B <+> C) +> A)")
    assert(show.show((Label("C") +> Label("B")) <+> Label("A")) == "((C +> B) <+> A)")
  }

  it("should be converted to string in set builder style") {
    val show = Label.setBuilderShow
    assert(show.show(Label.empty) == "")
    assert(show.show(Label("A")) == "A")
    assert(show.show(Label("A") <+> Label("B")) == "{A, B}")
    assert(show.show(Label("A") +> Label("B")) == "(A, B)")
    assert(show.show((Label("C") <+> Label("B")) <+> Label("A")) == "{A, B, C}")
    assert(show.show((Label("C") +> Label("B")) +> Label("A")) == "(C, B, A)")
    assert(show.show((Label("C") <+> Label("B")) +> Label("A")) == "({B, C}, A)")
    assert(show.show((Label("C") +> Label("B")) <+> Label("A")) == "{(C, B), A}")
  }
}
