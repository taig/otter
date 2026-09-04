package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.fixture.*
import zio.Scope
import zio.test.*

/** The claim the module exists to make: a document read through borer and the same document read through circe are the
  * same answer, value and violation tree alike.
  *
  * This is a differential test of the *adapter*, not of the interpreter. Both sides run `core`'s combinators over the
  * same schema, so what varies is only the code that turns a `Dom.Element` into what those combinators ask for -- and a
  * bug in `RecordDecoder`, `Fields` or `FieldDecoder` agrees with itself and passes here. Which is why
  * [[JsonBorerDecoderTest]] still states some answers absolutely.
  *
  * The corpus is each schema's canonical document plus one edit at a time, because a violation tree only gets
  * interesting where a document is *nearly* right. Random JSON fails at the top with a single type violation and says
  * nothing about accumulation, ordering, or paths.
  */
object JsonBorerAgreementTest extends ZIOSpecDefault:
  /** `A` is bound by the class rather than existential at the call site, so both decoders are known to produce the same
    * `Validated[Violations, A]` and the comparison type checks.
    */
  final private case class Subject[A](name: String, schema: Json.Reader[A], canonical: Doc):
    def agrees(doc: Doc): TestResult =
      assertTrue(
        JsonBorerDecoder.decode(schema, BorerDoc.toBorer(doc)) == JsonCirceDecoder.decode(schema, CirceDoc.toCirce(doc))
      )

    def documents: List[Doc] = canonical :: JsonBorerAgreementTest.mutations(canonical)

  private val book = Doc.Obj(List("title" -> Doc.Str("Dune"), "pages" -> Doc.Num("412"), "read" -> Doc.Bool(true)))

  private val note = Doc.Obj(List("title" -> Doc.Str("Dune"), "tag" -> Doc.Num("42")))

  private val subjects: List[Subject[?]] = List(
    Subject("book", json.book, book),
    Subject("omittedTag", json.omittedTag, note),
    Subject("nullableTag", json.nullableTag, note),
    Subject("nestedTag", json.nestedTag, Doc.Obj(List("tag" -> Doc.Num("42")))),
    Subject("genre", json.genre, Doc.Str("fiction")),
    Subject("shape", json.shape, Doc.Obj(List("radius" -> Doc.Num("1.5")))),
    Subject(
      "taggedShape",
      json.taggedShape,
      Doc.Obj(List("base" -> Doc.Num("1.5"), "height" -> Doc.Num("2.5")))
    ),
    Subject(
      "verdict",
      json.verdict,
      Doc.Obj(List("type" -> Doc.Str("deferred"), "reason" -> Doc.Str("later")))
    ),
    Subject("counter", json.counter, Doc.Str("42")),
    Subject(
      "editions",
      json.editions,
      Doc.Obj(List("3f2504e0-4f89-11d3-9a0c-0305e82c3301" -> Doc.Num("1")))
    ),
    Subject("printings", json.printings, Doc.Obj(List("1" -> Doc.Str("a"), "2" -> Doc.Str("b")))),
    Subject("catalogue", json.catalogue, Doc.Obj(List("9780441013593" -> Doc.Str("Dune")))),
    Subject(
      "tree",
      json.tree,
      Doc.Obj(
        List(
          "value" -> Doc.Num("1"),
          "children" -> Doc.Arr(List(Doc.Obj(List("value" -> Doc.Num("2"), "children" -> Doc.Arr(Nil)))))
        )
      )
    ),
    Subject("isbn", json.isbn, Doc.Str("9780441013593")),
    Subject("trimmedNote", json.trimmedNote, Doc.Obj(List("title" -> Doc.Str("  Dune  "), "tag" -> Doc.Num("42")))),
    Subject(
      "census",
      json.census,
      Doc.Obj(
        List(
          "first",
          "second",
          "third",
          "fourth",
          "fifth",
          "sixth",
          "seventh",
          "eighth",
          "ninth",
          "tenth",
          "eleventh",
          "twelfth",
          "thirteenth",
          "fourteenth",
          "fifteenth"
        ).map(name => name -> Doc.Str(name))
      )
    )
  )

  /** One edit at a time, which is where a violation tree says something. */
  private def mutations(doc: Doc): List[Doc] =
    val structural = doc match
      case Doc.Obj(values) =>
        Doc.Obj(values.drop(1)) ::
          Doc.Obj(values :+ ("unmentioned" -> Doc.Str("x"))) ::
          Doc.Obj(values.reverse) ::
          Doc.Obj(values.map((key, _) => key -> Doc.Null)) ::
          Doc.Obj(values.map((key, value) => key -> JsonBorerAgreementTest.retype(value))) ::
          values.indices.toList.map(index => Doc.Obj(values.updated(index, values(index).copy(_2 = Doc.Null))))
      case Doc.Arr(values) =>
        Doc.Arr(Nil) :: Doc.Arr(values.reverse) :: Doc.Arr(values.map(JsonBorerAgreementTest.retype)) :: Nil
      case leaf => Doc.Null :: JsonBorerAgreementTest.retype(leaf) :: Nil

    structural ++ JsonBorerAgreementTest.Lexemes.map(JsonBorerAgreementTest.renumber(doc, _))

  private def retype(doc: Doc): Doc = doc match
    case Doc.Null        => Doc.Str("null")
    case Doc.Bool(value) => Doc.Str(String.valueOf(value))
    case Doc.Num(lexeme) => Doc.Str(lexeme)
    case Doc.Str(_)      => Doc.Num("412")
    case Doc.Arr(values) => Doc.Obj(values.zipWithIndex.map((value, index) => index.toString -> value))
    case Doc.Obj(values) => Doc.Arr(values.map(_._2))

  /** Every number in the document rewritten as the same lexeme, which is what exercises `JsonBorerNumber`'s whole
    * table: `412` arrives as an `IntElem`, `412.0` as a `DoubleElem` and `1e400` as the raw text, and a schema reading
    * `Int` has to answer the same way for the first two and refuse the third exactly as circe does.
    */
  private def renumber(doc: Doc, lexeme: String): Doc = doc match
    case Doc.Num(_)      => Doc.Num(lexeme)
    case Doc.Arr(values) => Doc.Arr(values.map(JsonBorerAgreementTest.renumber(_, lexeme)))
    case Doc.Obj(values) => Doc.Obj(values.map((key, value) => key -> renumber(value, lexeme)))
    case leaf            => leaf

  /** [[DocTest.Lexemes]], which is every lexeme the two libraries read the same way. The two they do not are asserted
    * as divergences in [[JsonBorerDivergenceTest]] rather than smuggled in here, where they would only say that a known
    * difference is still there.
    *
    * A duplicated key is left out of [[mutations]] for the same reason: a record agrees, because both pick one
    * occurrence, and a dictionary does not, because circe's object collapses the duplicate and borer's keeps it.
    */
  private val Lexemes: List[String] = DocTest.Lexemes

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("JsonBorerAgreementTest")(
    subjects.map(subject => test(subject.name)(TestResult.allSuccesses(subject.documents.map(subject.agrees))))*
  )
