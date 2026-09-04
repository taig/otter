package io.taig.otter.fixture

/** A JSON document, as the one representation no interpreter owns.
  *
  * Not what the conformance suites use -- they state the document as text directly, and read it through each
  * interpreter's own parser. This is for a *differential* test, where the same document has to reach two interpreters
  * in two models and neither may be derived from the other: that derivation is itself the code under test, so its bugs
  * would cancel out and the comparison would pass while both were wrong. A hand built pair of trees is worse still, in
  * that it decides the number question in the fixture when which element a parser chooses for a given lexeme is the
  * whole crux.
  *
  * So a document is written down once as this, `render`ed, and each interpreter derives its own representation from the
  * text independently. That derivation lives beside the interpreter it belongs to -- `CirceDoc` in `core-json-circe`,
  * `BorerDoc` in `core-json-borer`. It lives here rather than beside either of them so that a third interpreter can
  * write its own differential test without depending on a second interpreter's test sources.
  *
  * A number is a **lexeme** rather than a `Double` for the same reason. borer's parser keeps `412` as an `IntElem`,
  * `412.0` as a `DoubleElem` and `1e400` as the raw string, while circe keeps all three as the text they were written
  * as -- and whether those three read as the same `Int` is the claim worth testing. A model holding a `Double` would
  * answer that question in the fixture instead of leaving it to the parsers.
  */
enum Doc:
  case Null
  case Bool(value: Boolean)
  case Num(lexeme: String)
  case Str(value: String)
  case Arr(values: List[Doc])
  case Obj(values: List[(String, Doc)])

object Doc:
  def render(doc: Doc): String = doc match
    case Doc.Null        => "null"
    case Doc.Bool(value) => String.valueOf(value)
    case Doc.Num(lexeme) => lexeme
    case Doc.Str(value)  => Doc.quote(value)
    case Doc.Arr(values) => values.map(Doc.render).mkString("[", ",", "]")
    case Doc.Obj(values) =>
      values.map((key, value) => Doc.quote(key) + ":" + Doc.render(value)).mkString("{", ",", "}")

  private def quote(value: String): String =
    val builder = new StringBuilder(value.length + 2)
    builder.append('"')

    value.foreach:
      case '"'                          => builder.append("\\\"")
      case '\\'                         => builder.append("\\\\")
      case '\b'                         => builder.append("\\b")
      case '\f'                         => builder.append("\\f")
      case '\n'                         => builder.append("\\n")
      case '\r'                         => builder.append("\\r")
      case '\t'                         => builder.append("\\t")
      case character if character < ' ' => builder.append("\\u%04x".format(character.toInt))
      case character                    => builder.append(character)

    builder.append('"').toString
